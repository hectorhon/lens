var express = require('express');
var router = express.Router();
const db = require('../db');
const usersDb = require('../db/users');

function route(handler) {
  return function(req, res, next) {
    handler(req, res).catch(err => {
      next(err);
    });
  }
}

router.get('/users', route(async (req, res) => {
  const usernames = await usersDb.list();
  res.render('users/index', { usernames });
}));

router.get('/users/create', route(async (req, res) => {
  res.render('users/create');
}));

router.post('/users/create', route(async (req, res) => {
  const username = req.body.username;
  const password = req.body.password;
  const passwordRepeat = req.body['password-repeat'];
  if (password !== passwordRepeat) {
    // TODO show flash message
    res.redirect('/users');
    return;
  }
  await usersDb.create(username, password);
  res.redirect('/users');
}));

router.post('/api/users/remove', route(async (req, res) => {
  const username = req.body.username;
  await usersDb.remove(username);
  res.status(200).end();
}));

module.exports = router;
