import React from 'react'
import PropTypes from 'prop-types'

class Row extends React.Component {
  render() {
    const { values } = this.props
    const cells = values.map((value, index) => (
      <td key={index}>
        {value}
      </td>
    ))
    return (
      <tr>
        {cells}
      </tr>
    )
  }
}

Row.propTypes = {
  values: PropTypes.arrayOf(PropTypes.any).isRequired
}

export default Row
