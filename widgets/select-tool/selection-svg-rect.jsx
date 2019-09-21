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
    const { selection, highlight, getSelectionRangeStart } = this.props
    const {
      x: selectionX, y: selectionY, width: selectionWidth, height: selectionHeight,
      name
    } = selection
    const x = selectionWidth > 0 ? selectionX : selectionX + selectionWidth
    const y = selectionHeight > 0 ? selectionY : selectionY + selectionHeight
    const width = Math.abs(selectionWidth)
    const height = Math.abs(selectionHeight)
    function getStrokeColour() {
      if (highlight) {
        return 'red'
      }
      if (selectionHeight <= 0 || selectionWidth <= 0) {
        return 'grey'
      }
      return 'black'
    }
    function getLabelStyle() {
      const style = {
        '-moz-user-select': 'none', // disable text selection when dragging handles
      }
      if (highlight) {
        style['font-weight'] = 'bold'
      }
      return style
    }
    function getRangeText() {
      const start = getSelectionRangeStart(selection.id) // zero-based
      const end = start + selection.numRows - 1 // zero-based
      return `${start + 1}-${end + 1}`
    }
    return (
      <g>
        <rect x={x}
              y={y}
              width={width}
              height={height}
              stroke={getStrokeColour()}
              strokeWidth={highlight ? 3 : 1}
              fill="none" />
        <text x={x}
              y={y + selectionHeight + 20}
              pointerEvents="none"
              style={getLabelStyle()}>
          {name} ({ getRangeText() })
        </text>
      </g>
    )
  }

  render() {
    const { selection } = this.props
    const {
      x: selectionX, y: selectionY, width: selectionWidth, height: selectionHeight,
      numRows, numColumns, spacingX, spacingY,
    } = selection
    const gridWidth = selection.getGridWidth()
    const gridHeight = selection.getGridHeight()
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
        {grids.map(grid => (
          <rect key={grid.index}
                x={grid.x}
                y={grid.y}
                width={grid.width}
                height={grid.height}
                stroke="black"
                fill="none" />
        ))}
        {this.renderFrame()}
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
  highlight: PropTypes.bool,
  getSelectionRangeStart: PropTypes.func.isRequired,
}

SelectionSvgRect.defaultProps = {
  highlight: false,
}

export default SelectionSvgRect
