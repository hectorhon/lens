const usersRepo = require('../db/users');
const sessionsRepo = require('../db/sessions');

async function migrate() {
  await usersRepo.migrateUp[0]();
  await sessionsRepo.migrateUp[0]();
  return;
}

migrate();
