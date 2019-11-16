import React from 'react'
import ReactDOM from 'react-dom'
import { createStore, applyMiddleware } from 'redux'
import { Provider } from 'react-redux'
import thunkMiddleware from 'redux-thunk'

import tableWidget from './reducers'
import Table from './table'
import { initialize } from './actions'

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
}, {
  id: 4,
  name: 'zxcv',
  age: 78,
  gender: 'M',
  weight: 80.80,
}]

// User provided function that should return a promise that contains
// refreshed data when the refresh button is pressed.
function refreshData(sortBy) {
  console.log('User provided refreshData function')
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
  console.log('User provided fetchMoreData function')
  return fetch('/api/table-data')
    .then(
      response => response.json(),
      error => console.log('An error occurred.', error)
    )
}

// User provided function to get the estimated total number of entries
function getEstimatedEntriesCount() {
  console.log('User provided getEstimatedEntriesCount function')
  return 4
}

const store = createStore(tableWidget, {
  data: initialData,
  columnNames: ['id', 'name', 'age', 'gender', 'weight'],
  pagination: { // FIXME: Make it so that can pick up default keys from pagination initialState
    currentPageNumber: 1,
    pageSize: 2,
    pagedData: [],
    pagedDataStartIndex: null,
    pagedDataEndIndex: null,
    sortBy: 'id',

    // The initial data does not have to include the entire
    // dataset. This field is used to show the user how many items there
    // are in the dataset.
    estimatedEntriesCount: 11,

    maxPageNumber: null,
  }
}, applyMiddleware(
  thunkMiddleware
))

store.dispatch(initialize())

ReactDOM.render(
  (
    <Provider store={store}>
      <Table refreshData={refreshData}
             fetchMoreData={fetchMoreData}
             getEstimatedEntriesCount={getEstimatedEntriesCount}
             pk="id" />
    </Provider>
  ),
  document.getElementById('root')
)

// const unsubscribe = store.subscribe(() => console.log(store.getState()))
// store.dispatch(selectCell(1, 2))
// unsubscribe()
