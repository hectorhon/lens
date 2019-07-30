const usersRepo = require('../db/users');
const sessionsRepo = require('../db/sessions');
const imagesRepo = require('../db/images');

async function migrate() {
  // await usersRepo.migrateUp[0]();
  // await sessionsRepo.migrateUp[0]();
  await imagesRepo.migrateUp[0]();
  return;
}

migrate();
