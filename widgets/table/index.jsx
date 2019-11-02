import React from 'react'
import ReactDOM from 'react-dom'
import { createStore } from 'redux'
import { Provider } from 'react-redux'

import tableWidget from './reducers'
import Table from './table'

const data = [{
  id: 1,
  name: 'asdf',
  age: 12,
  gender: 'M',
  weight: 50.50,
}, {
  id: 2,
  name: 'qwer',
  age: 34,
  gender: 'F',
  weight: 60.60,
}, {
  id: 3,
  name: 'uiop',
  age: 56,
  gender: 'M',
  weight: 70.70,
}]

const store = createStore(tableWidget, {
  columnNames: ['name', 'age', 'gender', 'weight']
})

ReactDOM.render(
  (
    <Provider store={store}>
      <Table data={data} pk="id" />
    </Provider>
  ),
  document.getElementById('root')
)

// const unsubscribe = store.subscribe(() => console.log(store.getState()))
// store.dispatch(selectCell(1, 2))
// unsubscribe()
