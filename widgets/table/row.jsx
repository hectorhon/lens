import React from 'react'
import PropTypes from 'prop-types'

import Cell from './cell'

class Row extends React.Component {
  render() {
    const { values, rowIndex } = this.props
    const cells = values.map((value, columnIndex) => (
      <Cell key={`${rowIndex},${columnIndex}`} // eslint-disable-line
            rowIndex={rowIndex}
            columnIndex={columnIndex}
            value={value} />
    ))
    return (
      <tr>
        {cells}
      </tr>
    )
  }
}

Row.propTypes = {
  values: PropTypes.arrayOf(PropTypes.any).isRequired,
  rowIndex: PropTypes.number.isRequired,
}

export default Row
