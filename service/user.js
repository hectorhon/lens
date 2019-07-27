const crypto = require('crypto');

const usersRepo = require('../db/users');

async function toHash(salt, password) {
  return new Promise(function(resolve, reject) {
    crypto.pbkdf2(
      password, salt, 100000, 64, 'sha512',
      (err, derivedKey) => {
        if (err) reject(err);
        resolve(derivedKey);
      });
  });
}

async function createUser(username, password) {
  const salt = await crypto.randomBytes(16);
  const hash = await toHash(salt, password);
  await usersRepo.create(username, salt, hash);
}

async function listUsers() {
  const usernames = usersRepo.list();
  return usernames;
}

async function verifyUser(username, password) {
  const { salt, hash } = await usersRepo.retrieve(username);
  if (salt && hash) {
    return toHash(salt, password).equals(hash);
  } else {
    return false;
  }
}

async function removeUser(username) {
  await usersRepo.remove(username);
}

module.exports = {
  createUser,
  listUsers,
  verifyUser,
  removeUser
};
