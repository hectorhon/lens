const db = require('../db');

async function mu0() {
  const sql =
        'create table users(' +
        'id serial primary key,' +
        'username varchar(60) unique not null,' +
        'salt bytea not null,' +
        'hash bytea not null,' +
        'deleted boolean not null default false)';
  return db.query(sql);
}

async function md0() {
  const sql = 'drop table users';
  return db.query(sql);
}



async function create(username, salt, hash) {
  const sql = 'insert into users(username, salt, hash) values ($1, $2, $3)';
  await db.query(sql, [username, salt, hash]);
}

// Return the (user) id, salt and hash of the given username
async function retrieve(username) {
  const sql =
        'select id, salt, hash from users where username = $1 ' +
        'and deleted = false';
  const result = await db.query(sql, [username]);
  return result.rowCount == 0 ? null : result.rows[0];
}

// List all usernames
async function list() {
  const sql = 'select username from users where deleted = false';
  const result = await db.query(sql);
  return result.rows.map(row => row.username);
}

async function remove(username) {
  const sql = 'update users set deleted = true where username = $1';
  await db.query(sql, [username]);
}

module.exports = {
  migrateUp: [mu0],
  migrateDown: [md0],
  create,
  retrieve,
  list,
  remove,
};
