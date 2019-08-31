import React from 'react'
import PropTypes from 'prop-types'

import Selection from './selection'

function SelectionSvgRect(props) {
  const { selection } = props
  const {
    x, y, width, height
  } = selection
  return (
    <rect x={x}
          y={y}
          width={width}
          height={height}
          stroke="black"
          fill="none" />
  )
}

SelectionSvgRect.propTypes = {
  selection: PropTypes.instanceOf(Selection).isRequired
}

export default SelectionSvgRect
