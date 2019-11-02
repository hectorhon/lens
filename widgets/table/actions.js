export const SELECT_CELL = 'SELECT_CELL'
export const MOVE_COLUMN = 'MOVE_COLUMN'

export function selectCell(rowIndex, columnIndex) {
  return {
    type: SELECT_CELL,
    rowIndex,
    columnIndex,
  }
}

export function moveColumn(columnToBeMoved, moveToBeforeThisColumn) {
  return {
    type: MOVE_COLUMN,
    columnToBeMoved,
    moveToBeforeThisColumn,
  }
}
