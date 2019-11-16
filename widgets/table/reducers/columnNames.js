import {
  MOVE_COLUMN,
} from '../actions'

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

export default columnNames
