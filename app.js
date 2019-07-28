var createError = require('http-errors');
var express = require('express');
var path = require('path');
var cookieParser = require('cookie-parser');
var logger = require('morgan');
var sassMiddleware = require('node-sass-middleware');

var indexRouter = require('./routes/index');
var usersRouter = require('./routes/users');
var loginRouter = require('./routes/login');
const userService = require('./service/user');

var app = express();

// view engine setup
app.set('views', path.join(__dirname, 'views'));
app.set('view engine', 'hbs');

app.use(logger('dev'));
app.use(express.json());
app.use(express.urlencoded({ extended: false }));
app.use(cookieParser());

// Load user session to res.locals.session, if present
const sessionMiddleware = express.Router();
sessionMiddleware.all('*', function(req, res, next) {
  const sessionId = req.cookies[loginRouter.SESSIONCOOKIENAME];
  if (sessionId) {
    userService.getSession(sessionId).then(session => {
      if (session != null) {
        res.locals.session = session;
      }
      next();
    });
  } else {
    next();
  }
});
app.use('/', sessionMiddleware);

app.use('/', loginRouter);

// Require authentication
const authenticationMiddleware = express.Router();
authenticationMiddleware.all('*', function(req, res, next) {
  if (res.locals.session) {
    next();
  } else {
    if (req.path.indexOf('/static/') == 0) {
      // static files, return 403 instead of login page
      res.status(403).end();
    } else {
      res.redirect('/login');
    }
  }
});
app.use('/', authenticationMiddleware);

// Stylesheets
app.use(sassMiddleware({
  src: path.join(__dirname, 'public'),
  dest: path.join(__dirname, 'public'),
  indentedSyntax: false, // true = .sass and false = .scss
  sourceMap: true
}));

// Static files
app.use('/static', express.static(path.join(__dirname, 'public')));

// CSRF token
const csrfMiddleware = express.Router();
csrfMiddleware.all('*', function(req, res, next) {
  if (req.method == 'GET') {
    // Expect no state changing operations via GET
    next();
  } else {
    if (req.is('application/json') ||
        req.is('application/x-www-form-urlencoded')) {
      const csrfToken = req.body.csrfToken;
      if (csrfToken === res.locals.session.data.csrfToken) {
        next();
      } else {
        res.status(403).send('Invalid CSRF token');
      }
    } else {
      res.status(400).send(
        'Unexpected content type when validating CSRF token');
    }
  }
});
app.use('/', csrfMiddleware);

// Remove loaded flash message from session
const clearFlashMessageMiddleware = express.Router();
clearFlashMessageMiddleware.all('*', function(req, res, next) {
  if (req.path.indexOf('/api/') == 0) {
    // Don't remove flash messages for API calls
    next();
  } else if (req.method == 'GET') {
    // Only remove flash messages for non-API GET calls
    userService.removeAllFlashMessages(res.locals.session.id)
      .then(() => next());
  } else {
    next();
  }
});
app.use('/', clearFlashMessageMiddleware);

// Application routes
app.use('/', indexRouter);
app.use('/', usersRouter);

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
