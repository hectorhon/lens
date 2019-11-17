import {
  REQUEST_DATA,
  RECEIVE_DATA,
} from '../actions'

function data(state = [], action) {
  switch (action.type) {
    case REQUEST_DATA:
      // TODO: show loading indicator or something
      return state

    case RECEIVE_DATA: {
      const { receivedData, clearOldDataFirst } = action

      const updatedState = clearOldDataFirst ? [] : state.slice()

      // Merge the received data to the state
      receivedData.forEach(entry => {
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

export default data
