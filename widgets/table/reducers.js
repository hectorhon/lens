// Don't mutate the state
// https://redux.js.org/basics/reducers

// We suggest you write independent small reducer functions that are
// each responsible for updates to a specific slice of state. We call
// this pattern “reducer composition”. A given action could be handled
// by all, some, or none of them.
// https://github.com/reduxjs/redux/blob/master/docs/faq/Actions.md

import { combineReducers } from 'redux'

import data from './reducers/data'
import columnNames from './reducers/columnNames'
import pagination from './reducers/pagination'
import activeCell from './reducers/activeCell'

const tableWidget = combineReducers({
  data,
  columnNames,
  pagination,
  activeCell,
})

export default tableWidget
