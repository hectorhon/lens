var express = require('express');
var router = express.Router();
const db = require('../db');

router.get('/', async (req, res, next) => {
  const result = await db.query('select 1');
  res.send(result.rows);
});

router.get('/2', async (req, res, next) => {
  const client = await db.getClient();
  const result = await client.query('select 2', []);
  client.release();
  res.send(result.rows);
});

module.exports = router;
