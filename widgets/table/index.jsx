import React from 'react'
import ReactDOM from 'react-dom'
import { createStore } from 'redux'
import { Provider } from 'react-redux'

import Table from './table'

const data = [{
  name: 'asdf',
  age: 12
}, {
  name: 'qwer',
  age: 34
}]

const store = createStore((state, action) => {
  if (typeof state === 'undefined') {
    const initialState = {
      data
    }
    return initialState
  }
  return state
})

ReactDOM.render(
  (
    <Provider store={store}>
      <Table data={data} columnNames={['name', 'age']} />
    </Provider>
  ),
  document.getElementById('root')
)
