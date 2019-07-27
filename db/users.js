const crypto = require('crypto');

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

async function toHash(salt, password) {
  return new Promise(function(resolve, reject) {
    crypto.pbkdf2(password, salt, 100000, 64, 'sha512', (err, derivedKey) => {
      if (err) reject(err);
      resolve(derivedKey);
    });
  });
}

async function create(username, password) {
  const salt = await crypto.randomBytes(16);
  const hash = await toHash(salt, password);
  const sql = 'insert into users(username, salt, hash) values ($1, $2, $3)';
  await db.query(sql, [username, salt, hash]);
}

// Return true if the supplied username and password is valid
async function verify(username, password) {
  const sql =
        'select salt, hash from users where username = $1 ' +
        'and deleted = false';
  const result = await db.query(sql, [username]);
  if (result.rowCount == 0) {
    return false;
  } else {
    const { salt, hash } = result.rows[0];
    const candidateHash = await toHash(salt, password);
    return candidateHash.equals(hash);
  }
}

// List all usernames
async function list() {
  const sql = 'select username from users where deleted = false';
  const result = await db.query(sql);
  return result.rows.map(row => row.username);
}

// Delete a user
async function remove(username) {
  const sql = 'update users set deleted = true where username = $1';
  await db.query(sql, [username]);
}

module.exports = {
  migrateUp: [mu0],
  migrateDown: [md0],
  create,
  verify,
  list,
  remove,
};
