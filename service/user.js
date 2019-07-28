const crypto = require('crypto');
const uuid = require('uuid');

const usersRepo = require('../db/users');
const sessionsRepo = require('../db/sessions');

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
  const entry = await usersRepo.retrieve(username);
  if (entry == null) return null;
  const candidateHash = await toHash(entry.salt, password);
  if (candidateHash.equals(entry.hash)) {
    const userId = entry.id;
    return userId;
  } else {
    return null;
  }
}

async function removeUser(username) {
  await usersRepo.remove(username);
}

// Attempt to log in the user. If successful, return a sessionId.
async function loginUser(username, password) {
  const userId = await verifyUser(username, password);
  if (userId) {
    const sessionId = await sessionsRepo.create(userId);
    return sessionId;
  } else {
    return null;
  }
}

async function getSession(sessionId) {
  const session = await sessionsRepo.retrieve(sessionId);
  if (session == null) {
    return null;
  } else {
    const { username, sessionData } = session;
    return {
      id: sessionId,
      username: username,
      data: sessionData
    };
  }
}

async function logoutUser(username) {
  await sessionsRepo.remove(username);
}

async function addFlashMessage(sessionId, message) {
  await sessionsRepo.update(sessionId, sessionData => {
    if (!sessionData.flashMessages) {
      sessionData.flashMessages = {};
    }
    const flashMessageId = uuid.v4();
    sessionData.flashMessages[flashMessageId] = message;
    return sessionData;
  });
}

async function removeFlashMessage(sessionId, flashMessageId) {
  await sessionsRepo.update(sessionId, sessionData => {
    if (!sessionData.flashMessages) {
      sessionData.flashMessages = {};
    }
    delete sessionData.flashMessages[flashMessageId];
    return sessionData;
  });
}

async function removeAllFlashMessages(sessionId) {
  await sessionsRepo.update(sessionId, sessionData => {
    sessionData.flashMessages = {};
    return sessionData;
  });
}

module.exports = {
  createUser,
  listUsers,
  verifyUser,
  removeUser,
  loginUser,
  getSession,
  logoutUser,
  addFlashMessage,
  removeAllFlashMessages,
};
