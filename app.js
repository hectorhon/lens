var createError = require('http-errors');
var express = require('express');
var path = require('path');
var cookieParser = require('cookie-parser');
var logger = require('morgan');
var hbs = require('hbs');
var sassMiddleware = require('node-sass-middleware');

const userService = require('./service/user');

var indexRouter = require('./routes/index');
var usersRouter = require('./routes/users');
var loginRouter = require('./routes/login');
var imagesRouter = require('./routes/images');

var app = express();

// view engine setup
app.set('views', path.join(__dirname, 'views'));
app.set('view engine', 'hbs');

// block and extend helpers - hbs/examples/extend/app.js
var blocks = {};
hbs.registerHelper('extend', function(name, context) {
  var block = blocks[name];
  if (!block) {
    block = blocks[name] = [];
  }
  block.push(context.fn(this));
});
hbs.registerHelper('block', function(name) {
  var val = (blocks[name] || []).join('\n');
  // clear the block
  blocks[name] = [];
  return val;
});
hbs.registerHelper('json', function(context) {
  return JSON.stringify(context);
});

app.use(logger('dev'));
app.use(express.json());
app.use(express.urlencoded({ extended: false }));
app.use(cookieParser());

// Allow source map requests, which are sent without cookies, in dev
// by exposing the entire static folder
if (app.get('env') === 'development') {
  app.use('/static', express.static(path.join(__dirname, 'static')));
}

// Load user session to res.locals.session, if present
// Otherwise generate a pre-session for the anonymous user
const sessionMiddleware = express.Router();
sessionMiddleware.all('*', function(req, res, next) {
  const sessionId = req.cookies[loginRouter.SESSIONCOOKIENAME];
  if (sessionId) {
    userService.getSession(sessionId).then(session => {
      if (session == null) {
        res.clearCookie(loginRouter.SESSIONCOOKIENAME);
        res.status(403).send('Invalid session id');
        return;
      }
      res.locals.session = session;
      next();
    });
  } else {
    userService.createPreSession().then(session => {
      res.locals.session = session;
      res.cookie(loginRouter.SESSIONCOOKIENAME, session.id, {
        httpOnly: true
      });
      next();
    });
  }
});
app.use('/', sessionMiddleware);

// CSRF token
const csrfMiddleware = express.Router();
csrfMiddleware.all('*', function(req, res, next) {
  if (req.method == 'GET') {
    // Expect no state changing operations via GET
    next();
  } else {
    if (req.is('application/json') ||
        req.is('application/x-www-form-urlencoded')) {
      if (req.body &&
          req.body.csrfToken === res.locals.session.data.csrfToken) {
        next();
      } else {
        res.status(403).send('Invalid CSRF token');
      }
    } else if (req.is('multipart/form-data')) {
      // defer CSRF check to the actual route
      next();
    } else {
      res.status(400).send('Missing CSRF token');
    }
  }
});
app.use('/', csrfMiddleware);

app.use('/', loginRouter);

// Require authentication
const authenticationMiddleware = express.Router();
authenticationMiddleware.all('*', function(req, res, next) {
  if (res.locals.session.username) {
    next();
  } else {
    if (req.path.startsWith('/static/')) {
      // static files, return 403 instead of login page
      res.status(403).end();
    } else {
      res.redirect('/login');
    }
  }
});
app.use('/', authenticationMiddleware);

// Stylesheets
app.use('/static/stylesheets', sassMiddleware({
  src: path.join(__dirname, 'static/stylesheets'),
  dest: path.join(__dirname, 'static/stylesheets'),
  indentedSyntax: false, // true = .sass and false = .scss
  sourceMap: true,
  sourceMapContents: true // avoid pointing to node_modules
}));

// Static files
app.use('/static', express.static(path.join(__dirname, 'static')));

// Remove loaded flash message from session
const clearFlashMessageMiddleware = express.Router();
clearFlashMessageMiddleware.all('*', function(req, res, next) {
  if (req.path.startsWith('/api/')) {
    // Don't remove flash messages for API calls
    next();
  } else if (req.method == 'GET') {
    // Only remove flash messages for non-API GET calls
    if (res.locals.session.data.flashMessages) {
      userService.removeAllFlashMessages(res.locals.session.id)
        .then(() => next());
    } else {
      next();
    }
  } else {
    next();
  }
});
app.use('/', clearFlashMessageMiddleware);

// Application routes
app.use('/', indexRouter);
app.use('/', usersRouter);
app.use('/', imagesRouter);

// catch 404 and forward to error handler
app.use(function(req, res, next) {
  next(createError(404));
});

// error handler
app.use(function(err, req, res, next) {
  // set locals, only providing error in development
  res.locals.message = err.message;
  res.locals.error = req.app.get('env') === 'development' ? err : {};

  // render the error page
  res.status(err.status || 500);
  res.render('error');
});

module.exports = app;
