import React from 'react'
import PropTypes from 'prop-types'

class ActiveSelectionSettingsPanel extends React.Component {
  increaseNumRows(delta) {
    const { updateFunction } = this.props
    updateFunction(selection => {
      selection.setNumRows(selection.numRows + delta)
    })
  }

  increaseNumColumns(delta) {
    const { updateFunction } = this.props
    updateFunction(selection => {
      selection.setNumColumns(selection.numColumns + delta)
    })
  }

  render() {
    const { getFunction } = this.props
    const selection = getFunction()
    return (
      <form>
        <div>
          <button type="button"
                  onClick={() => this.increaseNumRows(-1)}
                  disabled={selection.numRows <= selection.MIN_NUM_ROWS}>
            -
          </button>
          {selection.numRows} rows
          <button type="button"
                  onClick={() => this.increaseNumRows(1)}
                  disabled={selection.numRows >= selection.MAX_NUM_ROWS}>
            +
          </button>
        </div>
        <div>
          <button type="button"
                  onClick={() => this.increaseNumColumns(-1)}
                  disabled={selection.numColumns <= selection.MIN_NUM_COLUMNS}>
            -
          </button>
          {selection.numColumns} columns
          <button type="button"
                  onClick={() => this.increaseNumColumns(1)}
                  disabled={selection.numColumns >= selection.MAX_NUM_COLUMNS}>
            +
          </button>
        </div>
      </form>
    )
  }
}

ActiveSelectionSettingsPanel.propTypes = {
  getFunction: PropTypes.func.isRequired,
  updateFunction: PropTypes.func.isRequired,
}

export default ActiveSelectionSettingsPanel
