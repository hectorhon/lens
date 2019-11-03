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

router.get('/table', (req, res) => {
  res.render('table')
})

router.get('/api/table-data', (req, res) => {
  setTimeout(() => {
    const data = [{
      id: 1,
      name: 'asdf',
      age: 12,
      gender: 'M',
      weight: 51.51,
    }, {
      id: 2,
      name: 'qwer',
      age: 34,
      gender: 'F',
      weight: 61.61,
    }, {
      id: 3,
      name: 'uiop',
      age: 56,
      gender: 'M',
      weight: 71.71,
    }]
    res.json(data)
  }, 1000)
})

module.exports = router
