import React from 'react'
import ReactDOM from 'react-dom'
import { createStore, applyMiddleware } from 'redux'
import { Provider } from 'react-redux'
import thunkMiddleware from 'redux-thunk'

import tableWidget from './reducers'
import Table from './table'

const initialData = [{
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
  data: initialData,
  columnNames: ['name', 'age', 'gender', 'weight'],
}, applyMiddleware(
  thunkMiddleware
))

ReactDOM.render(
  (
    <Provider store={store}>
      <Table dataSourceUrl="/api/table-data" pk="id" />
    </Provider>
  ),
  document.getElementById('root')
)

// const unsubscribe = store.subscribe(() => console.log(store.getState()))
// store.dispatch(selectCell(1, 2))
// unsubscribe()
