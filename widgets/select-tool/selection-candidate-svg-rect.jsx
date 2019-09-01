import React from 'react'
import PropTypes from 'prop-types'

import Selection from './selection'

function SelectionCandidateSvgRect(props) {
  const { selection } = props
  const {
    x: selectionX, y: selectionY, width: selectionWidth, height: selectionHeight
  } = selection
  return (
    <g>
      <rect x={selectionX}
            y={selectionY}
            width={selectionWidth}
            height={selectionHeight}
            stroke="grey"
            fill="none" />
    </g>
  )
}

SelectionCandidateSvgRect.propTypes = {
  selection: PropTypes.instanceOf(Selection).isRequired
}

export default SelectionCandidateSvgRect
