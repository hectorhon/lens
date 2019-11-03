export const SELECT_CELL = 'SELECT_CELL'
export const MOVE_COLUMN = 'MOVE_COLUMN'
export const REQUEST_DATA = 'REQUEST_DATA'
export const RECEIVE_DATA = 'RECEIVE_DATA'

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

export function requestData() {
  return {
    type: REQUEST_DATA,
  }
}

export function receiveData(data) {
  return {
    type: RECEIVE_DATA,
    data,
  }
}

export function fetchData(url) {
  return dispatch => {
    dispatch(requestData())
    return fetch(url)
      .then(
        response => response.json(),
        error => console.log('An error occurred.', error)
      )
      .then(
        data => dispatch(receiveData(data))
      )
  }
}
