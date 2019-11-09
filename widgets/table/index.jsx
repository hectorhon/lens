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

// User provided function that should return a promise that contains
// refreshed data when the refresh button is pressed.
function refreshData(sortBy) {
  return fetch('/api/table-data')
    .then(
      response => response.json(),
      error => console.log('An error occurred.', error)
    )
}

// User provided function to load more data when needed
// sortBy: The column name that is currently being used to sort the data
// lastEntry: The last entry in the current state
// numEntriesToFetch: How many more entries to fetch
function fetchMoreData(sortBy, lastEntry, numEntriesToFetch) {
  return fetch('/api/table-data')
    .then(
      response => response.json(),
      error => console.log('An error occurred.', error)
    )
}

const store = createStore(tableWidget, {
  data: initialData,
  columnNames: ['id', 'name', 'age', 'gender', 'weight'],
  sortBy: 'id',
}, applyMiddleware(
  thunkMiddleware
))

ReactDOM.render(
  (
    <Provider store={store}>
      <Table refreshData={refreshData}
             fetchMoreData={fetchMoreData}
             pk="id" />
    </Provider>
  ),
  document.getElementById('root')
)

// const unsubscribe = store.subscribe(() => console.log(store.getState()))
// store.dispatch(selectCell(1, 2))
// unsubscribe()
