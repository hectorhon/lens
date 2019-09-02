import React from 'react'
import PropTypes from 'prop-types'

import Selection from './selection'

class SelectionSvgRect extends React.Component {
  constructor(props) {
    super(props)
    this.handleHandleMouseDown = this.handleHandleMouseDown.bind(this)
    this.handleHandleMouseUp = this.handleHandleMouseUp.bind(this)
  }

  handleHandleMouseDown(e, whichHandle) {
    e.stopPropagation()
    const { setCurrentlyEditing, selection } = this.props
    setCurrentlyEditing(selection.id, whichHandle)
  }

  handleHandleMouseUp(e) {
    e.stopPropagation()
    const { unsetCurrentlyEditing } = this.props
    unsetCurrentlyEditing()
  }

  renderFrame() {
    const { selection } = this.props
    const {
      x: selectionX, y: selectionY, width: selectionWidth, height: selectionHeight
    } = selection
    const x = selectionWidth > 0 ? selectionX : selectionX + selectionWidth
    const y = selectionHeight > 0 ? selectionY : selectionY + selectionHeight
    const width = Math.abs(selectionWidth)
    const height = Math.abs(selectionHeight)
    return (
      <rect x={x}
            y={y}
            width={width}
            height={height}
            stroke={selectionHeight > 0 && selectionWidth > 0 ? 'black' : 'grey'}
            fill="none" />
    )
  }

  render() {
    const { selection, numRows, numColumns } = this.props
    const {
      x: selectionX, y: selectionY, width: selectionWidth, height: selectionHeight
    } = selection
    const spacingX = 4
    const spacingY = 4
    const gridWidth = (selectionWidth - (numColumns - 1) * spacingX) / numColumns
    const gridHeight = (selectionHeight - (numRows - 1) * spacingY) / numRows
    const grids = []
    if (selectionWidth > 0 && selectionHeight > 0) {
      for (let i = 0; i < numRows; i += 1) {
        for (let j = 0; j < numColumns; j += 1) {
          grids.push({
            index: i * numColumns + j,
            x: selectionX + j * (gridWidth + spacingX),
            y: selectionY + i * (gridHeight + spacingY),
            width: gridWidth,
            height: gridHeight
          })
        }
      }
    }
    const handleSize = 16
    return (
      <g>
        {this.renderFrame()}
        {grids.map(grid => (
          <rect key={grid.index}
                x={grid.x}
                y={grid.y}
                width={grid.width}
                height={grid.height}
                stroke="black"
                fill="none" />
        ))}
        <rect x={selectionX - handleSize / 2}
              y={selectionY - handleSize / 2}
              width={handleSize}
              height={handleSize}
              stroke="black"
              fill="white"
              onDragStart={e => e.preventDefault()}
              onMouseDown={e => this.handleHandleMouseDown(e, 'upperleft')}
              onMouseUp={this.handleHandleMouseUp} />
        <rect x={selectionX + selectionWidth - handleSize / 2}
              y={selectionY + selectionHeight - handleSize / 2}
              width={handleSize}
              height={handleSize}
              stroke="black"
              fill="white"
              onDragStart={e => e.preventDefault()}
              onMouseDown={e => this.handleHandleMouseDown(e, 'lowerright')}
              onMouseUp={this.handleHandleMouseUp} />
      </g>
    )
  }
}

SelectionSvgRect.propTypes = {
  selection: PropTypes.instanceOf(Selection).isRequired,
  setCurrentlyEditing: PropTypes.func.isRequired,
  unsetCurrentlyEditing: PropTypes.func.isRequired,
  numRows: PropTypes.number.isRequired,
  numColumns: PropTypes.number.isRequired,
}

export default SelectionSvgRect
