import {
  SELECT_CELL,
} from '../actions'

function activeCell(state = {}, action) {
  switch (action.type) {
    case SELECT_CELL: {
      const { rowIndex, columnIndex } = action
      return {
        ...state,
        rowIndex,
        columnIndex,
      }
    }

    default:
      return state
  }
}

export default activeCell
