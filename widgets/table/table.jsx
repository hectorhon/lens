import React from 'react'
import PropTypes from 'prop-types'

import Row from './row'

class Table extends React.Component {
  getDataInArrayFormat() {
    const { data, columnNames } = this.props
    return data.map(entry => columnNames.map(columnName => entry[columnName]))
  }

  render() {
    const { columnNames } = this.props
    const headers = columnNames.map((columnName, index) => (
      <th key={index}>{columnName}</th>
    ))
    const data = this.getDataInArrayFormat()
    const rows = data.map((entry, index) => (
      <Row key={index} values={entry} />
    ))
    return (
      <table>
        <thead>
          <tr>
            {headers}
          </tr>
        </thead>
        <tbody>
          {rows}
        </tbody>
      </table>
    )
  }
}

Table.propTypes = {
  data: PropTypes.arrayOf(PropTypes.object).isRequired,
  columnNames: PropTypes.arrayOf(PropTypes.string)
}

Table.defaultProps = {
  columnNames: []
}

export default Table
