import React from 'react'
import PropTypes from 'prop-types'

import Selection from './selection'
import SelectionCandidateSvgRect from './selection-candidate-svg-rect'
import SelectionSvgRect from './selection-svg-rect'
import ActiveSelectionSettingsPanel from './active-selection-settings-panel'

class SelectTool extends React.Component {
  constructor(props) {
    super(props)
    this.state = {
      mouseDownPoint: null,
      mouseCurrentPoint: null,
      selections: [], // last array element is the active selection - svg z-index issue
      selectionsInCreationOrder: [], // array of selections in insertion order
      selectionNameIncrementalCounter: 1,
      currentlyEditing: null, // { selectionId, whichHandle } when user "drags" the handle
    }
    if (props.initialSelections) {
      const selections = props.initialSelections.map(object => Selection.fromJson(object))
      this.state.selections = selections
      this.state.selectionsInCreationOrder = selections.slice()
      this.state.selectionNameIncrementalCounter = 1 + props.initialSelections.length
    }
    this.handleMouseDown = this.handleMouseDown.bind(this)
    this.handleMouseMove = this.handleMouseMove.bind(this)
    this.handleMouseUp = this.handleMouseUp.bind(this)
    this.handleSave = this.handleSave.bind(this)
    this.addSelection = this.addSelection.bind(this)
    this.deleteSelection = this.deleteSelection.bind(this)
    this.deleteActiveSelection = this.deleteActiveSelection.bind(this)
    this.changeActiveSelection = this.changeActiveSelection.bind(this)
    this.handleSelectChange = this.handleSelectChange.bind(this)
    this.updateActiveSelection = this.updateActiveSelection.bind(this)
    this.getActiveSelection = this.getActiveSelection.bind(this)
    this.getSelectionRangeStart = this.getSelectionRangeStart.bind(this)
  }

  static getCoordinates(e) {
    const { left, top } = e.currentTarget.getBoundingClientRect()
    const x = e.clientX - left
    const y = e.clientY - top
    return { x, y }
  }

  getActiveSelection() {
    const { selections } = this.state
    const selection = selections[selections.length - 1]
    return selection
  }

  // Returns zero-based index
  getSelectionRangeStart(selectionId) {
    const { selectionsInCreationOrder } = this.state
    const index = selectionsInCreationOrder
      .findIndex(selection => selection.id === selectionId)
    const total = selectionsInCreationOrder
      .slice(0, index)
      .map(selection => selection.numRows)
      .reduce((sum, numRows) => sum + numRows, 0)
    return total
  }

  // f takes a selection updates it in place
  updateActiveSelection(f) {
    const { selections } = this.state
    const selection = this.getActiveSelection()
    f(selection)
    this.setState({
      selections: selections.slice()
    })
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

  isNameAlreadyTaken(name) {
    const { selections } = this.state
    return selections.map(selection => selection.name).includes(name)
  }

  generateNextName() {
    let { selectionNameIncrementalCounter } = this.state
    let name = `Selection ${selectionNameIncrementalCounter}`
    while (this.isNameAlreadyTaken(name)) {
      selectionNameIncrementalCounter += 1
      name = `Selection ${selectionNameIncrementalCounter}`
    }
    this.setState({
      selectionNameIncrementalCounter: selectionNameIncrementalCounter + 1,
    })
    return name
  }

  handleMouseUp(e) {
    const { mouseDownPoint, selectionNameIncrementalCounter } = this.state
    if (mouseDownPoint) {
      const { x: x1, y: y1 } = mouseDownPoint
      const { x: x2, y: y2 } = SelectTool.getCoordinates(e)
      const selection = Selection.create(x1, y1, x2, y2, this.generateNextName())
      const minHeight = 50
      const minWidth = 50
      if (selection.height > minHeight && selection.width > minWidth) {
        this.addSelection(selection)
      } else {
        // revert the change from this.getNextName()
        this.setState({ selectionNameIncrementalCounter })
      }
      this.setState({
        mouseDownPoint: null,
        mouseCurrentPoint: null,
      })
    }
  }

  handleSave() {
    const { save } = this.props
    const { selectionsInCreationOrder } = this.state
    save(selectionsInCreationOrder)
  }

  addSelection(selection) {
    const { selections, selectionsInCreationOrder } = this.state
    this.setState({
      selections: [...selections, selection],
      selectionsInCreationOrder: [...selectionsInCreationOrder, selection],
    })
  }

  deleteSelection(selectionId) {
    const { selections, selectionsInCreationOrder } = this.state
    this.setState({
      selections: selections.filter(s => s.id !== selectionId),
      selectionsInCreationOrder: selectionsInCreationOrder.filter(
        s => s.id !== selectionId
      ),
    })
  }

  deleteActiveSelection() {
    const selection = this.getActiveSelection()
    this.deleteSelection(selection.id)
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
      candidateSelection = Selection.create(x1, y1, x2, y2)
    }

    let settingsPanel
    if (selections.length) {
      const selectionSelector = (
        <select value={selections[selections.length - 1].id} onChange={this.handleSelectChange}>
          {selections.slice().sort((selection1, selection2) => {
            if (selection1.name < selection2.name) return -1
            if (selection1.name > selection2.name) return 1
            return 0
          }).map(selection => (
            <option key={selection.id} value={selection.id}>{selection.name}</option>
          ))}
        </select>
      )
      settingsPanel = (
        <>
          {selectionSelector}
          <ActiveSelectionSettingsPanel updateFunction={this.updateActiveSelection}
                                        getFunction={this.getActiveSelection}
                                        deleteFunction={this.deleteActiveSelection} />
        </>
      )
    }

    return (
      <>
        <div className="col-md-6">
          <div role="presentation"
               onMouseDown={this.handleMouseDown}
               onMouseMove={this.handleMouseMove}
               onMouseUp={this.handleMouseUp}
               style={{ position: 'relative', display: 'inline-block', border: '1px solid black' }}>
            <svg style={{ height: '100%', width: '100%', position: 'absolute' }}>
              {candidateSelection && <SelectionCandidateSvgRect selection={candidateSelection} />}
              {selections.map((selection, index) => (
                <SelectionSvgRect selection={selection}
                                  key={selection.id}
                                  highlight={index === selections.length - 1}
                                  setCurrentlyEditing={(selectionId, whichHandle) => {
                                    this.changeActiveSelection(selectionId)
                                    this.setState({
                                      currentlyEditing: { selectionId, whichHandle },
                                    })
                                  }}
                                  unsetCurrentlyEditing={() => {
                                    this.setState({ currentlyEditing: null })
                                    selections[selections.length - 1].ensurePositive()
                                  }}
                                  getSelectionRangeStart={this.getSelectionRangeStart} />
              ))}
            </svg>
            <img alt="Viewer"
                 src={imageSrc}
                 width="100%"
                 onDragStart={e => e.preventDefault()} />
          </div>
        </div>
        <div className="col-md-6">
          <form>
            <div className="d-flex align-items-center">
              <div className="flex-grow-1">
                <span>Click and drag on the image to mark important regions.</span>
              </div>
              <div>
                <button type="button"
                        className="btn btn-primary"
                        onClick={this.handleSave}>
                  Save
                </button>
              </div>
            </div>
            {settingsPanel}
          </form>
        </div>
      </>
    )
  }
}

SelectTool.propTypes = {
  imageSrc: PropTypes.string.isRequired,
  save: PropTypes.func.isRequired, // the save function takes a JSON object representing the state
  initialSelections: PropTypes.arrayOf(PropTypes.shape({
    id: PropTypes.string,
    order: PropTypes.number,
    name: PropTypes.string,
    x: PropTypes.number,
    y: PropTypes.number,
    width: PropTypes.number,
    height: PropTypes.number,
    numRows: PropTypes.number,
    numColumns: PropTypes.number,
    spacingX: PropTypes.number,
    spacingY: PropTypes.number,
  })),
}

SelectTool.defaultProps = {
  initialSelections: []
}

export default SelectTool
