export const SELECT_CELL = 'SELECT_CELL'
export function selectCell(rowIndex, columnIndex) {
  return {
    type: SELECT_CELL,
    rowIndex,
    columnIndex,
  }
}

export const MOVE_COLUMN = 'MOVE_COLUMN'
export function moveColumn(columnToBeMoved, moveToBeforeThisColumn) {
  return {
    type: MOVE_COLUMN,
    columnToBeMoved,
    moveToBeforeThisColumn,
  }
}

export const REQUEST_DATA = 'REQUEST_DATA'
export function requestData() {
  return {
    type: REQUEST_DATA,
  }
}

export const RECEIVE_DATA = 'RECEIVE_DATA'
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

export const GO_NEXT_PAGE = 'GO_NEXT_PAGE'
export function goNextPage(maxPageNumber) {
  return {
    type: GO_NEXT_PAGE,
    maxPageNumber,
  }
}

export const GO_PREVIOUS_PAGE = 'GO_PREVIOUS_PAGE'
export function goPreviousPage() {
  return {
    type: GO_PREVIOUS_PAGE
  }
}
