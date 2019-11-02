import { combineReducers } from 'redux'

import {
  SELECT_CELL
} from './actions'

function activeCell(state = {}, action) {
  switch (action.type) {
    case SELECT_CELL:
      return {
        ...state,
        rowIndex: action.rowIndex,
        columnIndex: action.columnIndex,
      }
    default:
      return state
  }
}

const tableWidget = combineReducers({
  activeCell
})

export default tableWidget
