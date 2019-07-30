const express = require('express');
const multer = require('multer');
const fs = require('fs');

const router = express.Router();

const UPLOAD_DIR = 'data/uploads/';
if (!fs.existsSync(UPLOAD_DIR)) {
  fs.mkdirSync(UPLOAD_DIR, { recursive: true });
}
const upload = multer({ dest: UPLOAD_DIR });

// CSRF check after reading the multipart request body
const csrfCheck = function(req, res, next) {
  if (req.body && req.body.csrfToken === res.locals.session.data.csrfToken) {
    next();
  } else {
    res.status(403).send('Invalid CSRF token');
    fs.unlink(req.file.destination + req.file.filename, err => {
      if (err) console.error(err);
    });
  }
}

route = handler => (req, res, next) => handler(req, res).catch(next);

router.get('/images', route(async (req, res) => {
  res.render('images/index');
}));

router.get('/images/upload', route(async (req, res) => {
  res.render('images/upload');
}));

router.post ('/images/upload', upload.single('file'), csrfCheck, route(async (req, res) => {
  res.redirect('/images');
}));

module.exports = router;
