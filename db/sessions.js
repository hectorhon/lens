const uuid = require('uuid')

const db = require('../db');

async function mu0() {
  const sql =
        'create table sessions(' +
        'id uuid primary key,' +
        'userId integer references users not null,' +
        'jsonData text not null)';
  await db.query(sql);
}

async function md0() {
  const sql = 'drop table sessions';
  return db.query(sql);
}



async function create(userId) {
  const sessionId = uuid.v4();
  const jsonData = {}; // start with empty session
  const sql =
        'insert into sessions (id, userId, jsonData) ' +
        'values ($1, $2, $3)';
  await db.query(sql, [sessionId, userId, jsonData]);
  return sessionId;
}

async function retrieve(sessionId) {
  const sql =
        'select s.jsonData, u.username ' +
        'from sessions as s ' +
        'inner join users as u ' +
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

function identity(x) { return x; }

// updater takes the old sessionData and returns the new sessionData
async function update(sessionId, updater = identity) {
  const client = await db.getClient();
  try {
    await client.query('begin');
    const retrieveSql =
          'select s.jsonData, u.username ' +
          'from sessions as s ' +
          'inner join users as u ' +
          'on s.userId = u.id ' +
          'where s.id = $1';
    const retrieveResult = await client.query(retrieveSql, [sessionId]);
    if (retrieveResult.rowCount == 0) {
      throw `Failed to find session ${sessionId} for update`;
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
  update,
  remove,
};
