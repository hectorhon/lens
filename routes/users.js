var express = require('express');
var router = express.Router();
const userService = require('../service/user');

function route(handler) {
  return function(req, res, next) {
    handler(req, res).catch(err => {
      next(err);
    });
  }
}

router.get('/users', route(async (req, res) => {
  const usernames = await userService.listUsers();
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
    await userService.addFlashMessage(
      res.locals.session.id,
      'Passwords do not match.');
    res.redirect('/users');
    return;
  }
  await userService.createUser(username, password);
  res.redirect('/users');
}));

router.post('/api/users/remove', route(async (req, res) => {
  const username = req.body.username;
  await userService.removeUser(username);
  res.status(200).end();
}));

module.exports = router;
