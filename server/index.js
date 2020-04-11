const express = require('express')
const contacts = require('./contacts/routes')

const app = express()
const port = 3000

app.use('/', contacts)

app.get('/', (req, res) => res.send('Hello World!'))

app.listen(port, () => console.log(`Example app listening at http://localhost:${port}`))
