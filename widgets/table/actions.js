export const SELECT_CELL = 'SELECT_CELL'

export function selectCell(rowIndex, columnIndex) {
  return {
    type: SELECT_CELL,
    rowIndex,
    columnIndex,
  }
}
