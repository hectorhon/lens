const express = require('express')

function asIntegerOrDefault(str, defaultNumber) {
  const n = Number(str)
  if (!Number.isInteger(n)) {
    if (str.trim()[0] === '-') {
      return -defaultNumber
    } else {
      return defaultNumber
    }
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

const allImageIds = [
  'rose.jpg',
  'rose_bRgo3sA.jpg',
  'ros.jpeg',
  'hand.jpeg',
  'eye.jpg',
  'pinkrose.jpg',
  'cat.jpeg',
  'camera.png',
]

// Get image ids using limit and offset
// router.get('/api/image-list', (req, res) => {
//   const pageNumber = asIntegerOrDefault(req.query.pageNumber, 1)
//   const pageSize = asIntegerOrDefault(req.query.pageSize, 2)
//   const offset = (pageNumber - 1) * pageSize
//   const limit = pageSize
//   res.json({
//     imageIds: allImageIds.slice(offset, offset + limit),
//     totalPages: Math.ceil(allImageIds.length / pageSize),
//   })
// })

// Get image ids using fromId and count
router.get('/api/image-list', (req, res) => {
  const { fromId } = req.query
  const count = asIntegerOrDefault(req.query.count, 5)

  if (fromId === 'undefined' || fromId === undefined) {
    res.json(allImageIds.slice(0, count))
    return
  }

  const index = allImageIds.indexOf(fromId)
  if (index === -1) {
    res.json([])
    return
  }

  if (count > 0) {
    res.json(allImageIds.slice(index + 1, index + 1 + count))
  } else if (count < 0) {
    res.json(allImageIds.slice(index - (-count), index))
  } else {
    res.json([])
  }
})

// Get total image-list count
router.get('/api/image-list-count', (req, res) => {
  res.json(allImageIds.length)
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
    }, {
      id: 4,
      name: 'zxcv',
      age: 78,
      gender: 'M',
      weight: 81.81,
    }, {
      id: 5,
      name: 'hjkl',
      age: 90,
      gender: 'F',
      weight: 91.91,
    }]
    res.json(data)
  }, 1000)
})

module.exports = router
