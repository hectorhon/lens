const express = require('express')
const router = express.Router()

router.get('/contacts', (req, res) => {
  res.send('hello contacts')
})

module.exports = router
