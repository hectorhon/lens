const db = require('../db');
const uuid = require('uuid');

async function mu0() {
  const sql =
        'create table images(' +
        'id uuid primary key, ' +
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
  const imageId = uuid.v4();
  const sql =
        'insert into images (id, filepath, size, originalName, uploadedBy) ' +
        'values ($1, $2, $3, $4, $5)';
  await db.query(sql, [imageId, filepath, size, originalName, uploadUserId]);
}

async function list() {
  const sql = 'select id, originalName from images';
  const result = await db.query(sql);
  return result.rows;
}

async function retrieve(imageId) {
  const sql =
        'select filepath, size, originalName, ' +
        'uploadedBy, uploadDate from images ' +
        'where id = $1';
  const result = await db.query(sql, [imageId]);
  return result.rowCount == 0 ? null : result.rows[0];
}

async function retrieveFilepath(imageId) {
  const sql = 'select filepath from images where id = $1';
  const result = await db.query(sql, [imageId]);
  return result.rowCount == 0 ? null : result.rows[0].filepath;
}

module.exports = {
  migrateUp: [mu0],
  migrateDown: [md0],
  record,
  list,
  retrieve,
  retrieveFilepath,
};
