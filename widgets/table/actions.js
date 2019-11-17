export const REPAGINATE = 'REPAGINATE'
export const REQUEST_DATA = 'REQUEST_DATA'
export const RECEIVE_DATA = 'RECEIVE_DATA'
export const RECEIVE_ESTIMATED_ENTRIES_COUNT = 'RECEIVE_ESTIMATED_ENTRIES_COUNT'

function repaginate() {
  return (dispatch, getState) => {
    const { data } = getState()
    dispatch({
      type: REPAGINATE,
      data,
    })
  }
}

function requestData() {
  return {
    type: REQUEST_DATA,
  }
}

function receiveData(receivedData, clearOldDataFirst = false) {
  return {
    type: RECEIVE_DATA,
    receivedData,
    clearOldDataFirst,
  }
}

function receiveEstimatedEntriesCount(count) {
  return (dispatch, getState) => {
    const { data } = getState()
    dispatch({
      type: RECEIVE_ESTIMATED_ENTRIES_COUNT,
      count,
      data,
    })
  }
}

function fetchMore(userProvidedFetchMoreDataFunction) {
  return dispatch => {
    dispatch(requestData())
    const promise = userProvidedFetchMoreDataFunction()
    return promise.then(
      data => {
        dispatch(receiveData(data))
        return data
      }
    )
  }
}

export const INITIALIZE = 'INITIALIZE'
export const SELECT_CELL = 'SELECT_CELL'
export const MOVE_COLUMN = 'MOVE_COLUMN'
export const GO_NEXT_PAGE = 'GO_NEXT_PAGE'
export const GO_PREVIOUS_PAGE = 'GO_PREVIOUS_PAGE'

export function initialize() {
  return dispatch => {
    dispatch(repaginate())
  }
}

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

export function refresh(userProvidedRefreshDataFunction) {
  return dispatch => {
    dispatch(requestData())
    const promise = userProvidedRefreshDataFunction()
    return promise.then(
      data => {
        dispatch(receiveData(data, true))
        dispatch(repaginate())
      }
    )
  }
}

export function goNextPage(
  userProvidedFetchMoreDataFunction,
  userProvidedGetEstimatedEntriesCountFunction
) {
  return (dispatch, getState) => {
    const { data, pagination } = getState()
    const { pagedDataEndIndex } = pagination

    const needMoreDataFromServer = pagedDataEndIndex === data.length
    if (needMoreDataFromServer) {
      Promise.all([
        dispatch(fetchMore(userProvidedFetchMoreDataFunction)),
        userProvidedGetEstimatedEntriesCountFunction()
      ]).then(([newData, count]) => {
        dispatch(receiveEstimatedEntriesCount(count))
        if (newData.length > data.length) {
          dispatch({
            type: GO_NEXT_PAGE,
          })
        }
        dispatch(repaginate())
      })
    } else {
      dispatch({
        type: GO_NEXT_PAGE,
      })
      dispatch(repaginate())
    }
  }
}

export function goPreviousPage() {
  return dispatch => {
    dispatch({
      type: GO_PREVIOUS_PAGE,
    })
    dispatch(repaginate())
  }
}
