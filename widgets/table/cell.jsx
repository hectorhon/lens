import React from 'react'
import PropTypes from 'prop-types'
import { connect } from 'react-redux'

import * as actions from './actions'

class Cell extends React.Component {
  render() {
    const {
      rowIndex, columnIndex, value, isSelected, selectCell
    } = this.props
    const innerDivStyle = {
      border: '1px solid transparent',
      padding: '0.5em',
    }
    if (isSelected) {
      innerDivStyle.border = '1px solid black'
    }
    return (
      <td style={{ padding: '0px' }}>
        <div role="presentation" style={innerDivStyle} onClick={() => selectCell(rowIndex, columnIndex)}>
          {value}
        </div>
      </td>
    )
  }
}

Cell.propTypes = {
  rowIndex: PropTypes.number.isRequired,
  columnIndex: PropTypes.number.isRequired,
  value: PropTypes.oneOfType([
    PropTypes.string,
    PropTypes.number,
  ]),
  isSelected: PropTypes.bool.isRequired,
  selectCell: PropTypes.func.isRequired,
}

Cell.defaultProps = {
  value: ''
}

const mapStateToProps = (state, ownProps) => {
  const { activeCell } = state
  return {
    isSelected: activeCell
             && activeCell.rowIndex === ownProps.rowIndex
             && activeCell.columnIndex === ownProps.columnIndex
  }
}

const mapDispatchToProps = {
  selectCell: actions.selectCell
}

export default connect(mapStateToProps, mapDispatchToProps)(Cell)
