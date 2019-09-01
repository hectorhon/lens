import React from 'react'
import PropTypes from 'prop-types'

import Selection from './selection'
import SelectionCandidateSvgRect from './selection-candidate-svg-rect'
import SelectionSvgRect from './selection-svg-rect'

class SelectTool extends React.Component {
  constructor(props) {
    super(props)
    this.state = {
      mouseDownPoint: null,
      mouseCurrentPoint: null,
      selections: [], // last array element is the active selection - svg z-index issue
      currentlyEditing: null, // { selectionId, whichHandle } when user "drags" the handle
    }
    this.handleMouseDown = this.handleMouseDown.bind(this)
    this.handleMouseMove = this.handleMouseMove.bind(this)
    this.handleMouseUp = this.handleMouseUp.bind(this)
    this.addSelection = this.addSelection.bind(this)
    this.changeActiveSelection = this.changeActiveSelection.bind(this)
    this.handleSelectChange = this.handleSelectChange.bind(this)
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
    const { currentlyEditing, mouseDownPoint, selections } = this.state
    const { x, y } = SelectTool.getCoordinates(e)
    if (currentlyEditing) {
      const selection = selections[selections.length - 1]
      if (currentlyEditing.whichHandle === 'upperleft') {
        selection.updateUpperLeft(x, y)
      } else if (currentlyEditing.whichHandle === 'lowerright') {
        selection.updateLowerRight(x, y)
      }
      this.setState({
        selections: selections.slice()
      })
    } else if (mouseDownPoint) {
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
      const selection = new Selection(x1, y1, x2, y2)
      const minSize = 100
      if (selection.height > minSize && selection.width > minSize) {
        this.addSelection(selection)
      }
      this.setState({
        mouseDownPoint: null,
        mouseCurrentPoint: null,
      })
    }
  }

  addSelection(selection) {
    const { selections } = this.state
    this.setState({
      selections: [...selections, selection]
    })
  }

  changeActiveSelection(selectionId) {
    const { selections } = this.state
    const front = selections.filter(s => s.id !== selectionId)
    const last = selections.find(s => s.id === selectionId)
    this.setState({
      selections: [...front, last]
    })
  }

  handleSelectChange(e) {
    const selectionId = e.target.value
    this.changeActiveSelection(selectionId)
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
            {candidateSelection && <SelectionCandidateSvgRect selection={candidateSelection} />}
            {selections.map(selection => (
              <SelectionSvgRect selection={selection}
                                key={selection.id}
                                setCurrentlyEditing={(selectionId, whichHandle) => {
                                  this.changeActiveSelection(selectionId)
                                  this.setState({
                                    currentlyEditing: { selectionId, whichHandle },
                                  })
                                }}
                                unsetCurrentlyEditing={() => {
                                  this.setState({ currentlyEditing: null })
                                  selections[selections.length - 1].ensurePositive()
                                }} />
            ))}
          </svg>
          <img alt="Viewer"
               src={imageSrc}
               onDragStart={e => e.preventDefault()} />
        </div>
        {selections.length && (
          <select value={selections[selections.length - 1].id} onChange={this.handleSelectChange}>
            {selections.map(selection => (
              <option key={selection.id} value={selection.id}>{selection.id}</option>
            ))}
          </select>
        )}
      </div>
    )
  }
}

SelectTool.propTypes = {
  imageSrc: PropTypes.string.isRequired
}

export default SelectTool
