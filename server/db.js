const { Pool } = require('pg')

const pool = new Pool({
  host: '/var/run/postgresql',
})

module.exports = {
  query: (text, params) => pool.query(text, params),
}
