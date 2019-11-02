import React from 'react'
import PropTypes from 'prop-types'

import Row from './row'

class Table extends React.Component {
  getDataInArrayFormat() {
    const { data, columnNames } = this.props
    return data.map(entry => columnNames.map(columnName => entry[columnName]))
  }

  render() {
    const { data, pk, columnNames } = this.props
    const headers = columnNames.map(columnName => (
      <th key={columnName}>{columnName}</th>
    ))
    const rows = data.map((entry, index) => {
      const values = columnNames.map(columnName => entry[columnName])
      return (
        <Row key={data[index][pk]} rowIndex={index} values={values} />
      )
    })
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
  pk: PropTypes.string.isRequired,
  columnNames: PropTypes.arrayOf(PropTypes.string),
}

Table.defaultProps = {
  columnNames: [],
}

export default Table
