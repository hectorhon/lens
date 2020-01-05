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

let allImageIds = [
  '1.jpg',
  '2.jpg',
  '3.jpg',
  '4.jpg',
  '5.jpg',
  '6.jpg',
  '7.jpg',
  '8.jpg',
]

router.get('/api/image-list', (req, res) => {
  const pageNumber = asIntegerOrDefault(req.query.pageNumber, 1)
  const pageSize = asIntegerOrDefault(req.query.pageSize, 2)
  const offset = (pageNumber - 1) * pageSize
  const limit = pageSize
  res.json({
    imageIds: allImageIds.slice(offset, offset + limit),
    totalPages: Math.ceil(allImageIds.length / pageSize),
  })
})

router.post('/api/image-list/delete', (req, res) => {
  const imageIdsToDelete = req.body

  // Determine new page number if bookmark is given
  // The new page number is the page containing the image just before the bookmark
  const { bookmark: bookmarkImageId, pageSize } = req.query
  let newPageNumber
  if (bookmarkImageId && pageSize) {
    const otherImageIdsToDelete = imageIdsToDelete.filter(id => id !== bookmarkImageId)
    const index = allImageIds
      .filter(id => !otherImageIdsToDelete.includes(id))
      .indexOf(bookmarkImageId)
    newPageNumber = Math.floor(index / pageSize) + 1
  }

  // Perform the deletion
  allImageIds = allImageIds.filter(id => !imageIdsToDelete.includes(id))

  res.json({
    newImagesCount: allImageIds.length,
    newPageNumber,
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
