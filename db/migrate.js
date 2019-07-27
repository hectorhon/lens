const usersDb = require('../db/users');

async function migrate() {
  await usersDb.migrateDown[0]();
  await usersDb.migrateUp[0]();
  // await usersDb.create('asdf', '1234');
  return;
}

migrate();
