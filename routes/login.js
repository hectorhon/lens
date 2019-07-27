const express = require('express');
const router = express.Router();
const userService = require('../service/user');

const SESSIONCOOKIENAME = 's';

function route(handler) {
  return function(req, res, next) {
    handler(req, res).catch(err => {
      next(err);
    });
  }
}

router.get('/login', route(async (req, res) => {
  if (res.locals.session) { // already logged in
    res.redirect('/');
  } else {
    res.locals.layout = false;
    res.render('login');
  }
}));

router.post('/login', route(async (req, res) => {
  const username = req.body.username;
  const password = req.body.password;
  const sessionId = await userService.loginUser(username, password);
  if (sessionId) {
    res.cookie(SESSIONCOOKIENAME, sessionId);
    res.redirect('/');
  } else {
    res.redirect('/login');
  }
}));

router.post('/logout', route(async (req, res) => {
  await userService.logoutUser(res.locals.session.username);
  res.clearCookie(SESSIONCOOKIENAME);
  res.locals.layout = false;
  res.render('loggedOut');
}));

router.SESSIONCOOKIENAME = SESSIONCOOKIENAME;

module.exports = router;
