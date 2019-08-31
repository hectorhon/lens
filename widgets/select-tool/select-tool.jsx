import React from 'react'
import PropTypes from 'prop-types'

import Selection from './selection'
import SelectionSvgRect from './selection-svg-rect'

class SelectTool extends React.Component {
  constructor(props) {
    super(props)
    this.state = {
      mouseDownPoint: undefined,
      mouseCurrentPoint: undefined,
      selections: [],
    }
    this.handleMouseDown = this.handleMouseDown.bind(this)
    this.handleMouseMove = this.handleMouseMove.bind(this)
    this.handleMouseUp = this.handleMouseUp.bind(this)
  }

  static getCoordinates(e) {
    const { left, top } = e.currentTarget.getBoundingClientRect()
    const x = e.clientX - left
    const y = e.clientY - top
    return { x, y }
  }

  handleMouseDown(e) {
    const { x, y } = SelectTool.getCoordinates(e)
    this.setState({
      mouseDownPoint: { x, y },
      mouseCurrentPoint: { x, y }
    })
  }

  handleMouseMove(e) {
    const { mouseDownPoint } = this.state
    if (mouseDownPoint) {
      const { x, y } = SelectTool.getCoordinates(e)
      this.setState({
        mouseCurrentPoint: { x, y }
      })
    }
  }

  handleMouseUp(e) {
    const { mouseDownPoint } = this.state
    if (mouseDownPoint) {
      const { x: x1, y: y1 } = mouseDownPoint
      const { x: x2, y: y2 } = SelectTool.getCoordinates(e)
      this.addSelection(new Selection(x1, y1, x2, y2))
      this.setState({
        mouseDownPoint: undefined,
        mouseCurrentPoint: undefined,
      })
    }
  }

  addSelection(selection) {
    const { selections } = this.state
    this.setState({
      selections: [...selections, selection]
    })
  }

  render() {
    const { imageSrc } = this.props
    const { mouseDownPoint, mouseCurrentPoint, selections } = this.state
    let candidateSelection
    if (mouseDownPoint && mouseCurrentPoint) {
      const { x: x1, y: y1 } = mouseDownPoint
      const { x: x2, y: y2 } = mouseCurrentPoint
      candidateSelection = new Selection(x1, y1, x2, y2)
    }
    return (
      <div>
        <div role="presentation"
             onMouseDown={this.handleMouseDown}
             onMouseMove={this.handleMouseMove}
             onMouseUp={this.handleMouseUp}
             style={{ position: 'relative', display: 'inline-block' }}>
          <svg style={{ height: '100%', width: '100%', position: 'absolute' }}>
            {candidateSelection && <SelectionSvgRect selection={candidateSelection} />}
            {selections.map(selection => (
              <SelectionSvgRect selection={selection} key={selection.id} />
            ))}
          </svg>
          <img alt="Viewer"
               src={imageSrc}
               onDragStart={e => e.preventDefault()} />
        </div>
        <p>{ JSON.stringify(mouseDownPoint)}, {JSON.stringify(mouseCurrentPoint) }</p>
      </div>
    )
  }
}

SelectTool.propTypes = {
  imageSrc: PropTypes.string.isRequired
}

export default SelectTool
