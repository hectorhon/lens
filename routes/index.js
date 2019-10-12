const express = require('express')

function asIntegerOrDefault(str, defaultNumber) {
  const n = Number(str)
  if (!Number.isInteger(n)) {
    return defaultNumber
  }
  return n
}

const router = express.Router()

router.get('/', (req, res) => {
  res.render('index', { title: 'Widgets workbench' })
})

router.get('/select-tool', (req, res) => {
  res.render('select-tool')
})

router.get('/image-gallery', (req, res) => {
  res.render('image-gallery')
})

router.get('/api/image-list', (req, res) => {
  const allImageIds = [
    'rose.jpeg',
    'ros.jpeg',
    'hand.jpeg',
    'eye.jpg',
    'pinkrose.jpg',
    'cat.jpeg',
    'camera.png',
  ]
  const pageNumber = asIntegerOrDefault(req.query.pageNumber, 1)
  const pageSize = asIntegerOrDefault(req.query.pageSize, 2)
  const offset = (pageNumber - 1) * pageSize
  const limit = pageSize
  res.json({
    imageIds: allImageIds.slice(offset, offset + limit),
    totalPages: Math.ceil(allImageIds.length / pageSize),
  })
})

module.exports = router
