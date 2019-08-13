import React from 'react';
import ReactDOM from 'react-dom';
import PropTypes from 'prop-types';
import update from 'immutability-helper';
import uuid from 'uuid';

import SvgContext from './svg-context.js';
import SvgSelectRegion from './svg-select-region.jsx';
import ImageProperties from './image-properties.jsx';
import SelectRegionProperties from './select-region-properties.jsx';

import css from './select-tool.scss';

class SelectTool extends React.Component {

  constructor(props) {
    super(props);
    this.state = {
      candidatePoint1: null,
      candidatePoint2: null,
      selections: [],
      lockFocus: false, // to avoid entering another SelectRegion while e.g. resizing
      svgContext: {},   // https://reactjs.org/docs/context.html#caveats
    };
    this.svgRef = React.createRef();
  }

  componentDidMount() {
    const rect = this.svgRef.current.getBoundingClientRect();
    const baseX = rect.x + window.scrollX;
    const baseY = rect.y + window.scrollY;
    this.setState({
      svgContext: {
        baseX, baseY,
        getRelativeMousePosition: e => {
          var x = e.pageX - baseX;
          var y = e.pageY - baseY;
          return { x, y };
        },
        svgRef: this.svgRef
      }
    });
  }

  render() {
    return (
      <div className="row select-tool">
          <div className="col-sm-6">
              <div className="select-tool__image-display">
                  <SvgContext.Provider value={this.state.svgContext}>
                      <svg ref={this.svgRef}
                           onMouseDown={this.handleMouseDown}
                           {...(this.state.candidatePoint1 ?
                                {
                                  onMouseMove: this.handleMouseMove,
                                  onMouseUp: this.handleMouseUp
                                } : { }
                           )} >
                          {this.state.candidatePoint1 && this.state.candidatePoint2 &&
                           <SvgSelectRegion x1={this.state.candidatePoint1.x}
                                            y1={this.state.candidatePoint1.y}
                                            x2={this.state.candidatePoint2.x}
                                            y2={this.state.candidatePoint2.y}
                                            isCandidate={true} />
                          }
                          {this.state.selections.map((selection, index, selections) => (
                            <SvgSelectRegion key={selection.id}
                                             {...selection}
                                             onDragStart={() => this.setState({ lockFocus: true })}
                                             onRegionCoordsChanged={this.modifySelection}
                                             onDragStop={() => this.setState({ lockFocus: false })}
                                             {...(!this.state.lockFocus ?
                                                  { onMouseEnter: this.focusSelectRegion } : { }
                                             )}
                                             highlight={index === selections.length - 1} />
                          ))}
                      </svg>
                  </SvgContext.Provider>
                  <img src={`/images/get?id=${this.props.image.id}&fullsize=true`}
                       className="img-fluid rounded mx-auto d-block" />
              </div>
          </div>
          <div className="col-sm-6">
              <ImageProperties {...this.props.image} />
              <SelectRegionProperties selections={this.state.selections} />
          </div>
      </div>
    );
  }

  handleMouseDown = e => {
    const { x, y } = this.state.svgContext.getRelativeMousePosition(e);
    this.setState({
      candidatePoint1: { x, y },
      candidatePoint2: { x, y },
      lockFocus: true,
    });
  }

  handleMouseMove = e => {
    this.setState({
      candidatePoint2: this.state.svgContext.getRelativeMousePosition(e)
    });
  }

  handleMouseUp = e => {
    const MIN_WIDTH = 20;
    const MIN_HEIGHT = 20;
    const width = Math.abs(this.state.candidatePoint1.x - this.state.candidatePoint2.x);
    const height = Math.abs(this.state.candidatePoint1.y - this.state.candidatePoint2.y);
    if (width > MIN_WIDTH && height > MIN_HEIGHT) {
      this.addSelection(this.state.candidatePoint1.x,
                        this.state.candidatePoint1.y,
                        this.state.candidatePoint2.x,
                        this.state.candidatePoint2.y);
    }
    this.setState({
      candidatePoint1: null,
      candidatePoint2: null,
      lockFocus: false,
    });
  }

  handleMouseLeave = e => {
    this.setState({
      candidatePoint1: null,
      candidatePoint2: null,
      lockFocus: false,
    });
  }

  modifySelection = (id, x1, y1, x2, y2) => {
    this.setState(prevState => {
      const index = prevState.selections.findIndex(selection => selection.id == id);
      return {
        selections: update(prevState.selections, { [index]: { $merge: { x1, y1, x2, y2 } } })
      };
    });
  }

  addSelection = (x1, y1, x2, y2) => {
    this.setState(prevState => {
      return {
        selections: [...prevState.selections, { id: uuid.v4(), x1, y1, x2, y2 }]
      }
    });
  }

  // Need this because svg z-index is not supported
  // https://bugzilla.mozilla.org/show_bug.cgi?id=360148
  // https://bugs.chromium.org/p/chromium/issues/detail?id=670177
  // Allow user to focus on another SelectRegion by painting it last
  focusSelectRegion = id => {
    this.setState(prevState => {
      const index = prevState.selections.findIndex(selection => selection.id == id);
      const target = prevState.selections[index];
      return {
        selections: [...update(prevState.selections, { $splice: [[index, 1]] }), target]
      }
    });
  }

}

SelectTool.propTypes = {
  image: PropTypes.exact({
    id: PropTypes.string,
    size: PropTypes.number,
    originalName: PropTypes.string,
    uploadDate: PropTypes.instanceOf(Date)
  })
};

image.uploadDate = new Date(image.uploadDate);

ReactDOM.render(
  <SelectTool image={image} />,
  document.querySelector('#select-tool-container')
);
