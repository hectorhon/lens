const express = require('express')

const repo = require('./repository')

const router = express.Router()

router.get('/contacts', async (req, res) => {
  const contacts = await repo.select()
  res.render('contacts/index', {
    title: 'Contacts',
    contacts
  })
})

router.get('/contacts/add', async (req, res) => {
  res.render('contacts/add', {
    title: 'Contacts - Add',
  })
})

router.post('/contacts/add', async (req, res) => {
  const { name } = req.body
  await repo.insert(name)
  res.redirect('/contacts')
})

router.get('/contacts/edit', async (req, res) => {
  const { id } = req.query
  const contact = await repo.get(id)
  res.render('contacts/edit', {
    title: 'Contacts - Edit',
    contact
  })
})

router.post('/contacts/edit', async (req, res) => {
  const { id } = req.query
  const { name } = req.body
  await repo.update(id, name)
  res.redirect('/contacts')
})

router.get('/contacts/delete', async (req, res) => {
  const { id } = req.query
  const contact = await repo.get(id)
  res.render('contacts/delete', {
    title: 'Contacts - Delete',
    contact
  })
})

router.post('/contacts/delete', async (req, res) => {
  const { id } = req.query
  await repo.remove(id)
  res.redirect('/contacts')
})

module.exports = router
