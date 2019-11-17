import {
  REPAGINATE,
  GO_NEXT_PAGE,
  GO_PREVIOUS_PAGE,
  RECEIVE_ESTIMATED_ENTRIES_COUNT,
} from '../actions'

const initialState = {
  currentPageNumber: 1,
  pageSize: 10,
  pagedData: [],
  pagedDataStartIndex: null,
  pagedDataEndIndex: null,
  sortBy: null,
  estimatedEntriesCount: null,
  maxPageNumber: null,
}

function pagination(state = initialState, action) {
  const { type } = action
  switch (type) {
    case REPAGINATE: {
      const {
        currentPageNumber,
        pageSize,
        estimatedEntriesCount,
      } = state

      const { data } = action

      // If the last page is not full, obviously there are no more
      // items and the current estimate is wrong
      const newEstimatedEntriesCount = data.length % pageSize > 0
        ? data.length
        : estimatedEntriesCount

      const maxPageNumber = Math.ceil(newEstimatedEntriesCount / pageSize)
      const newPageNumber = Math.min(currentPageNumber, Math.ceil(data.length / pageSize))

      const pagedDataStartIndex = (newPageNumber - 1) * pageSize
      let pagedDataEndIndex = pagedDataStartIndex + pageSize
      const pagedData = data.slice(pagedDataStartIndex, pagedDataEndIndex)
      pagedDataEndIndex = pagedDataStartIndex + pagedData.length // the actual value

      return {
        ...state,
        estimatedEntriesCount: newEstimatedEntriesCount,
        maxPageNumber,
        currentPageNumber: newPageNumber,
        pagedData,
        pagedDataStartIndex,
        pagedDataEndIndex,
      }
    }

    case RECEIVE_ESTIMATED_ENTRIES_COUNT: {
      const { data, count } = action
      const estimatedEntriesCount = Math.max(data.length, count)
      return {
        ...state,
        estimatedEntriesCount,
      }
    }

    case GO_NEXT_PAGE: {
      const { currentPageNumber, maxPageNumber } = state
      const newPageNumber = Math.min(currentPageNumber + 1, maxPageNumber)
      return {
        ...state,
        currentPageNumber: newPageNumber,
      }
    }

    case GO_PREVIOUS_PAGE: {
      const { currentPageNumber } = state
      const newPageNumber = Math.max(currentPageNumber - 1, 1)
      return {
        ...state,
        currentPageNumber: newPageNumber,
      }
    }

    default: return state
  }
}

export default pagination
