const uuid = require('uuid')

const db = require('../db')

async function initialize() {
  db.query('create table lens_contacts (id uuid primary key, name varchar)')
}

async function select() {
  const result = await db.query('select * from lens_contacts')
  return result.rows
}

async function insert(name) {
  const id = uuid.v4()
  await db.query(
    'insert into lens_contacts (id, name) values ($1, $2)',
    [id, name]
  )
}

async function get(id) {
  const result = await db.query(
    'select * from lens_contacts where id = $1',
    [id]
  )
  return result.rows[0]
}

async function update(id, name) {
  await db.query(
    'update lens_contacts set name = $2 where id = $1',
    [id, name]
  )
}

async function remove(id) {
  await db.query(
    'delete from lens_contacts where id = $1',
    [id]
  )
}

module.exports = {
  initialize, select, insert, get, update, remove
}
