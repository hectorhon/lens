const express = require('express')
const bodyParser = require('body-parser')

const contacts = require('./contacts/routes')

const app = express()
const port = 3000

app.set('view engine', 'ejs')
app.set('views', '.')
app.set('view options', {
  root: __dirname
})

app.use(bodyParser.urlencoded({ extended: false }))
app.use(bodyParser.json())

app.use('/', contacts)

app.get('/', async (req, res) => {
  res.render('index', {
    number: new Date()
  })
})

app.listen(port, () => {
  console.log(`Example app listening at http://localhost:${port}`)
})
