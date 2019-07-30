const db = require('../db');

async function mu0() {
  const sql =
        'create table images(' +
        'id serial primary key, ' +
        'filepath varchar not null, ' +
        'size integer not null, ' +
        'originalName varchar not null, ' +
        'uploadedBy uuid references users, ' +
        'uploadDate timestamp not null default now(), ' +
        'deleted boolean not null default false)';
  return db.query(sql);
}

async function md0() {
  const sql = 'drop table images';
  return db.query(sql);
}



async function record(filepath, size, originalName, uploadUserId) {
  const sql =
        'insert into images (filepath, size, originalName, uploadedBy) ' +
        'values ($1, $2, $3, $4)';
  await db.query(sql, [filepath, size, originalName, uploadUserId]);
}

module.exports = {
  migrateUp: [mu0],
  migrateDown: [md0],
  record,
};
