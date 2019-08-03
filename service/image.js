const fs = require('fs');
const sharp = require('sharp');
const imagesRepo = require('../db/images');

const UPLOAD_DIR = 'data/uploads/';
const THUMBNAIL_DIR = 'data/thumbnails/';
const THUMBNAIL_MAX_SIZE = { x: 200, y: 200 };

function ensureDirExists(dir) {
  if (!fs.existsSync(dir)) {
    fs.mkdirSync(dir, { recursive: true });
  }
}
ensureDirExists(UPLOAD_DIR);
ensureDirExists(THUMBNAIL_DIR);

async function listImages() {
  return imagesRepo.list();
}

async function saveNewImageEntry(filepath, size, originalFilename,
                                 uploadUserId) {
  return imagesRepo.record(filepath, size, originalFilename,
                           uploadUserId);
}

async function getImageFilepath(imageId) {
  return imagesRepo.retrieveFilepath(imageId);
}

// Thumbnails are generated on demand and "cached" in THUMBNAIL_DIR
async function getThumbnailFilepath(imageId) {
  const filepath = await imagesRepo.retrieveFilepath(imageId);
  const thumbFilepath = filepath.replace(UPLOAD_DIR, THUMBNAIL_DIR);
  return new Promise(function(resolve, reject) {
    fs.access(thumbFilepath, fs.constants.F_OK, (err) => {
      if (err) {
        sharp(filepath)
          .resize(THUMBNAIL_MAX_SIZE.x, THUMBNAIL_MAX_SIZE.y, {
            fit: 'inside',
          })
          .toFile(thumbFilepath)
          .then(() => {
            resolve(thumbFilepath);
          });
      } else {
        resolve(thumbFilepath);
      }
    });
  });
}

async function getImageEntry(imageId) {
  const imageEntry = await imagesRepo.retrieve(imageId);
  // return { id: imageId, ...imageEntry };
  return imageEntry;
}

module.exports = {
  UPLOAD_DIR,
  listImages,
  saveNewImageEntry,
  getImageFilepath,
  getThumbnailFilepath,
  getImageEntry,
};
