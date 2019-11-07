// Don't mutate the state
// https://redux.js.org/basics/reducers

import { combineReducers } from 'redux'

import {
  SELECT_CELL,
  MOVE_COLUMN,
  REQUEST_DATA,
  RECEIVE_DATA,
  GO_NEXT_PAGE,
  GO_PREVIOUS_PAGE,
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

function columnNames(state = [], action) {
  switch (action.type) {
    case MOVE_COLUMN: {
      const { columnToBeMoved, moveToBeforeThisColumn } = action

      // 1. Determine the movement direction
      // This helps to make the drag/drop to behaviour intuitive
      let direction
      {
        const columnToBeMovedIndex = state.indexOf(columnToBeMoved)
        const destinationIndex = state.indexOf(moveToBeforeThisColumn)
        if (columnToBeMovedIndex > destinationIndex) {
          direction = 'move to left'
        } else if (columnToBeMovedIndex < destinationIndex) {
          direction = 'move to right'
        }
      }

      // 2. Remove the columnToBeMoved
      const reorderedColumnNames = state.filter(
        columnName => columnName !== columnToBeMoved
      )

      // 3. Insert the columnToBeMoved in the new destination
      const destinationIndex = reorderedColumnNames.indexOf(moveToBeforeThisColumn)
      if (direction === 'move to left') {
        reorderedColumnNames.splice(destinationIndex, 0, columnToBeMoved)
      } else if (direction === 'move to right') {
        reorderedColumnNames.splice(destinationIndex + 1, 0, columnToBeMoved)
      } else {
        // can't determine direction; do nothing
        return state
      }

      return reorderedColumnNames
    }
    default:
      return state
  }
}

function data(state = [], action) {
  switch (action.type) {
    case REQUEST_DATA:
      // TODO: show loading indicator or something
      return state
    case RECEIVE_DATA: {
      // Merge the received data to the state
      const updatedState = state.slice()
      action.data.forEach(entry => {
        const indexOfExistingEntry = updatedState.findIndex(e => e.id === entry.id)
        if (indexOfExistingEntry === -1) {
          // new entry, append to end
          updatedState.push(entry)
        } else {
          // existing entry, update it
          updatedState.splice(indexOfExistingEntry, 1, entry)
        }
      })
      return updatedState
    }
    default:
      return state
  }
}

function pagination(state = {
  currentPageNumber: 1,
  pageSize: 2,
}, action) {
  const { currentPageNumber } = state
  switch (action.type) {
    case GO_NEXT_PAGE: {
      const { maxPageNumber } = action
      return {
        ...state,
        currentPageNumber: Math.min(currentPageNumber + 1, maxPageNumber),
      }
    }
    case GO_PREVIOUS_PAGE:
      return {
        ...state,
        currentPageNumber: Math.max(currentPageNumber - 1, 1),
      }
    default: return state
  }
}

const tableWidget = combineReducers({
  activeCell,
  columnNames,
  data,
  pagination,
})

export default tableWidget
