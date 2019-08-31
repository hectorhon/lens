import React from 'react'
import PropTypes from 'prop-types'

import Selection from './selection'

function SelectionSvgRect(props) {
  const { selection } = props
  const {
    x: selectionX, y: selectionY, width: selectionWidth, height: selectionHeight
  } = selection
  const rows = 20
  const columns = 4
  const spacingX = 4
  const spacingY = 4
  const gridWidth = (selectionWidth - (columns - 1) * spacingX) / columns
  const gridHeight = (selectionHeight - (rows - 1) * spacingY) / rows
  const grids = []
  for (let i = 0; i < rows; i += 1) {
    for (let j = 0; j < columns; j += 1) {
      grids.push({
        x: selectionX + j * (gridWidth + spacingX),
        y: selectionY + i * (gridHeight + spacingY),
        width: gridWidth,
        height: gridHeight
      })
    }
  }
  return (
    <g>
      <rect x={selectionX}
            y={selectionY}
            width={selectionWidth}
            height={selectionHeight}
            stroke="black"
            fill="none" />
      {grids.map((grid, index) => (
        <rect key={index}
              x={grid.x}
              y={grid.y}
              width={grid.width}
              height={grid.height}
              stroke="black"
              fill="none" />
      ))}
    </g>
  )
}

SelectionSvgRect.propTypes = {
  selection: PropTypes.instanceOf(Selection).isRequired
}

export default SelectionSvgRect
