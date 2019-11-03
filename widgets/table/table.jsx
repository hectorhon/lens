import React from 'react'
import PropTypes from 'prop-types'
import { connect } from 'react-redux'

import * as actions from './actions'
import Row from './row'

class Table extends React.Component {
  constructor(props) {
    super(props)
    this.handleDragStart = this.handleDragStart.bind(this)
    this.handleDragOver = this.handleDragOver.bind(this)
    this.handleDrop = this.handleDrop.bind(this)
  }

  getDataInArrayFormat() {
    const { data, columnNames } = this.props
    return data.map(entry => columnNames.map(columnName => entry[columnName]))
  }

  handleDragStart(e) {
    e.dataTransfer.setData('text/plain', e.target.innerText)
    e.dataTransfer.dropEffect = 'move'
  }

  handleDragOver(e) {
    e.preventDefault() // required for the <th> to become a drop zone
  }

  handleDrop(e) {
    e.preventDefault()
    const { moveColumn } = this.props
    const columnToBeMoved = e.dataTransfer.getData('text/plain')
    const moveToBeforeThisColumn = e.target.innerText
    moveColumn(columnToBeMoved, moveToBeforeThisColumn)
  }

  render() {
    const {
      dataSourceUrl, pk, data, columnNames, fetchData
    } = this.props
    const headers = columnNames.map(columnName => (
      <th key={columnName}
          draggable="true"
          onDragStart={this.handleDragStart}
          onDragOver={this.handleDragOver}
          onDrop={this.handleDrop}>
        {columnName}
      </th>
    ))
    const rows = data.map((entry, index) => {
      const values = columnNames.map(columnName => entry[columnName])
      return (
        <Row key={data[index][pk]} rowIndex={index} values={values} />
      )
    })
    return (
      <div className="react-table">
        <button type="button" onClick={() => fetchData(dataSourceUrl)}>Refresh</button>
        <table style={{ userSelect: 'none' }}>
          <thead>
            <tr>
              {headers}
            </tr>
          </thead>
          <tbody>
            {rows}
          </tbody>
        </table>
      </div>
    )
  }
}

Table.propTypes = {
  dataSourceUrl: PropTypes.string.isRequired,
  pk: PropTypes.string.isRequired,
  data: PropTypes.arrayOf(PropTypes.object).isRequired,
  columnNames: PropTypes.arrayOf(PropTypes.string),
  moveColumn: PropTypes.func.isRequired,
  fetchData: PropTypes.func.isRequired,
}

Table.defaultProps = {
  columnNames: [],
}

const mapStateToProps = state => ({
  data: state.data,
  columnNames: state.columnNames
})

const mapDispatchToProps = {
  moveColumn: actions.moveColumn,
  fetchData: actions.fetchData,
}

export default connect(mapStateToProps, mapDispatchToProps)(Table)
