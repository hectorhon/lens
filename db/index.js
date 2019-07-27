const { Pool } = require('pg');

const pool = new Pool({
  user: 'lens',
  host: '/var/run/postgresql', // unix domain socket
  database: 'lens',
  password: 'lens',
  max: 2,
});

module.exports = {

  query: async (text, params) => {
    const start = Date.now();
    const result = await pool.query(text, params);
    const duration = Date.now() - start;
    console.log('Executed query', {
      text, duration, rows: result.rowCount
    });
    return result;
  },

  getClient: async () => {
    const client = await pool.connect();

    const query = client.query;
    client.query = async function() {
      client.lastQuery = arguments[0];
      return query.apply(client, arguments);
    };

    const timeout = setTimeout(() => {
      console.error('A client has been checked out for more than 5 seconds!')
      console.error(`The last executed query on this client was: ${client.lastQuery}`)
    }, 5000);

    const release = client.release;
    client.release = function() {
      clearTimeout(timeout);
      release.apply(client, arguments);
      client.query = query;
      client.release = release;
    };

    return client;
  },
}
