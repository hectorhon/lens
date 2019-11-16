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
    this.handleRefreshButtonClicked = this.handleRefreshButtonClicked.bind(this)
    this.handlePreviousPageButtonClicked = this.handlePreviousPageButtonClicked.bind(this)
    this.handleNextPageButtonClicked = this.handleNextPageButtonClicked.bind(this)
  }

  handleDragStart(e) { // eslint-disable-line
    e.dataTransfer.setData('text/plain', e.target.innerText)
    e.dataTransfer.dropEffect = 'move'
  }

  handleDragOver(e) { // eslint-disable-line
    e.preventDefault() // required for the <th> to become a drop zone
  }

  handleDrop(e) {
    e.preventDefault()
    const { moveColumn } = this.props
    const columnToBeMoved = e.dataTransfer.getData('text/plain')
    const moveToBeforeThisColumn = e.target.innerText
    moveColumn(columnToBeMoved, moveToBeforeThisColumn)
  }

  handleRefreshButtonClicked() {
    const { refreshData, refresh } = this.props
    refresh(refreshData)
  }

  handlePreviousPageButtonClicked() {
    const { goPreviousPage } = this.props
    goPreviousPage()
  }

  handleNextPageButtonClicked() {
    const { goNextPage, fetchMoreData, getEstimatedEntriesCount } = this.props
    goNextPage(fetchMoreData, getEstimatedEntriesCount)
  }

  render() {
    const {
      pk,
      columnNames,
      // pagination related
      currentPageNumber, pagedData, pagedDataStartIndex, pagedDataEndIndex,
      sortBy, estimatedEntriesCount, maxPageNumber,
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

    const rows = pagedData.map((entry, index) => {
      const values = columnNames.map(columnName => entry[columnName])
      return (
        <Row key={pagedData[index][pk]} rowIndex={index} values={values} />
      )
    })

    return (
      <div className="react-table">
        <button type="button" onClick={this.handleRefreshButtonClicked}>Refresh</button>

        <p>
          Showing {pagedDataStartIndex + 1}-{pagedDataEndIndex}{' '}
          of {estimatedEntriesCount} results.
        </p>

        <p>Current sort: {sortBy}</p>

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

        <button type="button"
                onClick={this.handlePreviousPageButtonClicked}
                disabled={currentPageNumber === 1}>
          Previous page
        </button>

        <button type="button"
                onClick={this.handleNextPageButtonClicked}
                disabled={currentPageNumber === maxPageNumber}>
          Next page
        </button>
      </div>
    )
  }
}

Table.propTypes = {
  // From props
  refreshData: PropTypes.func.isRequired,
  fetchMoreData: PropTypes.func.isRequired,
  getEstimatedEntriesCount: PropTypes.func.isRequired,
  pk: PropTypes.string.isRequired,

  // From mapStateToProps
  columnNames: PropTypes.arrayOf(PropTypes.string),
  currentPageNumber: PropTypes.number.isRequired,
  pagedData: PropTypes.arrayOf(PropTypes.object).isRequired,
  pagedDataStartIndex: PropTypes.number.isRequired,
  pagedDataEndIndex: PropTypes.number.isRequired,
  sortBy: PropTypes.string.isRequired,
  estimatedEntriesCount: PropTypes.number.isRequired,
  maxPageNumber: PropTypes.number.isRequired,

  // From mapDispatchToProps
  refresh: PropTypes.func.isRequired,
  moveColumn: PropTypes.func.isRequired,
  goNextPage: PropTypes.func.isRequired,
  goPreviousPage: PropTypes.func.isRequired,
}

Table.defaultProps = {
  columnNames: [],
}

const mapStateToProps = state => ({
  columnNames: state.columnNames,
  currentPageNumber: state.pagination.currentPageNumber,
  pagedData: state.pagination.pagedData,
  pagedDataStartIndex: state.pagination.pagedDataStartIndex,
  pagedDataEndIndex: state.pagination.pagedDataEndIndex,
  sortBy: state.pagination.sortBy,
  estimatedEntriesCount: state.pagination.estimatedEntriesCount,
  maxPageNumber: state.pagination.maxPageNumber,
})

const mapDispatchToProps = {
  refresh: actions.refresh,
  moveColumn: actions.moveColumn,
  goNextPage: actions.goNextPage,
  goPreviousPage: actions.goPreviousPage,
}

export default connect(mapStateToProps, mapDispatchToProps)(Table)
