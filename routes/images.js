const fs = require('fs');
const express = require('express');
const multer = require('multer');
const uuid = require('uuid');
const imageService = require('../service/image');

const router = express.Router();

const upload = multer({
  storage: multer.diskStorage({
    destination: (req, file, cb) => {
      cb(null, imageService.UPLOAD_DIR);
    },
    filename: (req, file, cb) => {
      const split = file.originalname.split('.');
      if (split.length <= 1) {  // no extension in uploaded file
        var extension = '';
      } else {
        var extension = split[split.length - 1];
        if (extension.length > 4) { // allow up to 4 chars extension only
          extension = '';
        }
      }
      const imageId = uuid.v4();
      // Multer will simply overwrite the file if same name!  So, need
      // the Date.now(). This means will always need to refer to db
      // anyway when getting the physical file path. So, no need to
      // make uuid in physical file path same as db key.
      // file.imageId = imageId;
      cb(null, Date.now() + '-' + imageId + (extension ? '.' + extension : ''));
    }
  })
});

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
  const imageEntries = await imageService.listImages();
  res.render('images/index', { entries: imageEntries });
}));

router.get('/images/upload', route(async (req, res) => {
  res.render('images/upload');
}));

router.post ('/images/upload', upload.single('file'), csrfCheck, route(async (req, res) => {
  // Example req.file
  // {
  //   fieldname: 'file',
  //   originalname: 'casey-horner-75_s8iWHKLs-unsplash.jpg',
  //   encoding: '7bit',
  //   mimetype: 'image/jpeg',
  //   destination: 'data/uploads/',
  //   filename: '1564805031685-0cd660b5-f20f-427a-923c-4eb02a5130cb.jpg',
  //   path:
  //   'data/uploads/1564805031685-0cd660b5-f20f-427a-923c-4eb02a5130cb.jpg',
  //   size: 4125672
  // }
  await imageService.saveNewImageEntry(
    req.file.path,
    req.file.size,
    req.file.originalname,
    res.locals.session.userId);
  res.redirect('/images');
}));

/* GET the actual image */
router.get('/images/get', route(async(req, res) => {
  const imageId = req.query.id;
  if (req.query.fullsize) {
    var filepath = await imageService.getImageFilepath(imageId);
  } else {
    var filepath = await imageService.getThumbnailFilepath(imageId);
  }
  res.sendFile(filepath, { root: process.cwd() });
}));

router.get('/images/view', route(async(req, res) => {
  const imageId = req.query.id;
  const imageEntry = await imageService.getImageEntry(imageId);
  res.render('images/view', {
    image: {
      id: imageId,
      size: imageEntry.size,
      originalName: imageEntry.originalName,
      uploadDate: imageEntry.uploadDate
    },
  });
}));

module.exports = router;
