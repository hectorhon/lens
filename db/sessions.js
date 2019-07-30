const uuid = require('uuid')

const db = require('../db');

async function mu0() {
  const sql =
        'create table sessions(' +
        'id uuid primary key,' +
        'userId uuid references users, ' +
        'jsonData text not null)';
  await db.query(sql);
}

async function md0() {
  const sql = 'drop table sessions';
  return db.query(sql);
}



// Create a pre-session, not associated with any user. Returns the sessionId.
async function create(initialSessionData = {}) {
  const userId = null;
  const sessionId = uuid.v4();
  const sql =
        'insert into sessions (id, userId, jsonData) ' +
        'values ($1, $2, $3)';
  await db.query(sql, [sessionId, userId, initialSessionData]);
  return sessionId;
}

async function retrieve(sessionId) {
  const sql =
        'select s.jsonData, u.username ' +
        'from sessions as s ' +
        'left outer join users as u ' +
        'on s.userId = u.id ' +
        'where s.id = $1';
  const result = await db.query(sql, [sessionId]);
  if (result.rowCount == 0) {
    return null;
  } else {
    return {
      username: result.rows[0].username,
      sessionData: JSON.parse(result.rows[0].jsondata) // note the small caps
    };
  }
}

// Associate the (pre-)session with a user. Returns true if successful
async function associateUser(sessionId, userId) {
  const sql =
        'update sessions as s set userId = $1 ' +
        'where s.id = $2 and userId is null';
  const result = await db.query(sql, [userId, sessionId]);
  return result.rowCount === 1;
}

function identity(x) { return x; }

// updater takes the old sessionData and returns the new sessionData
async function updateData(sessionId, updater = identity) {
  const client = await db.getClient();
  try {
    await client.query('begin');
    const retrieveSql =
          'select s.jsonData, u.username ' +
          'from sessions as s ' +
          'left outer join users as u ' +
          'on s.userId = u.id ' +
          'where s.id = $1';
    const retrieveResult = await client.query(retrieveSql, [sessionId]);
    if (retrieveResult.rowCount == 0) {
      throw new Error(`Failed to find session ${sessionId} to update session data`);
    }
    const session = {
      username: retrieveResult.rows[0].username,
      sessionData: JSON.parse(retrieveResult.rows[0].jsondata) // note the small caps
    };
    const updatedSessionData = updater(session.sessionData);
    const updateSql = 'update sessions set jsonData = $1 where id = $2';
    const result = await client.query(updateSql, [
      JSON.stringify(updatedSessionData),
      sessionId
    ]);
    await client.query('commit');
  } catch (e) {
    await client.query('rollback');
    throw e;
  } finally {
    client.release();
  }
}

async function remove(username) {
  const sql =
        'delete from sessions as s where s.userId in ' +
        '(select id from users where username = $1);'
  await db.query(sql, [username]);
}

module.exports = {
  migrateUp: [mu0],
  migrateDown: [md0],
  create,
  retrieve,
  associateUser,
  updateData,
  remove,
};
