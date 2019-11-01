import React from 'react'
import ReactDOM from 'react-dom'

import Table from './table'

const data = [{
  name: 'asdf',
  age: 12
}, {
  name: 'qwer',
  age: 34
}]

ReactDOM.render(
  <Table data={data} columnNames={['name', 'age']} />,
  document.getElementById('root')
)
