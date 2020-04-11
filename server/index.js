const express = require('express')
const contacts = require('./contacts/routes')

const app = express()
const port = 3000

app.set('view engine', 'ejs')
app.set('views', '.')

app.use('/', contacts)

app.get('/', (req, res) => {
  res.render('index', {
    number: new Date()
  })
})

app.listen(port, () => {
  console.log(`Example app listening at http://localhost:${port}`)
})
