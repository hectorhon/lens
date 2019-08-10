import React from 'react';
import ReactDOM from 'react-dom';
import PropTypes from 'prop-types';

import SvgSelectRegion from './svg-select-region.jsx';
import ImageProperties from './image-properties.jsx';

import css from './select-tool.scss';

class SelectTool extends React.Component {

  constructor(props) {
    super(props);
    this.state = {
      candidatePoint1: null,
      candidatePoint2: null,
      selections: []
    };
  }

  render() {
    return (
      <div className="row select-tool thing">
          <div className="col-sm-6">
              <div className="select-tool__image-display"
                   onMouseDown={(e) => this.handleMouseDown(e)}
                   onMouseMove={(e) => this.handleMouseMove(e)}
                   onMouseUp={(e) => this.handleMouseUp(e)}
                   onMouseLeave={(e) => this.handleMouseLeave(e)} >
                  <svg>
                      {this.state.candidatePoint1 && this.state.candidatePoint2 &&
                       <SvgSelectRegion x1={this.state.candidatePoint1.x}
                                        y1={this.state.candidatePoint1.y}
                                        x2={this.state.candidatePoint2.x}
                                        y2={this.state.candidatePoint2.y} />}
                      {this.state.selections.map(selection => (
                        <SvgSelectRegion {...selection} />
                      ))}
                  </svg>
                  <img src={`/images/get?id=${this.props.image.id}&fullsize=true`}
                       className="img-fluid rounded mx-auto d-block" />
              </div>
          </div>
          <div className="col-sm-6">
              <ImageProperties {...this.props.image} />
          </div>
      </div>
    );
  }

  handleMouseDown(e) {
    e.preventDefault();
    const { x, y } = this.getRelativeMousePosition(e);
    this.setState({
      candidatePoint1: { x, y },
      candidatePoint2: { x, y },
    });
  }

  handleMouseMove(e) {
    e.preventDefault();
    if (this.state.candidatePoint1) {
      this.setState({
        candidatePoint2: this.getRelativeMousePosition(e)
      });
    }
  }

  handleMouseUp(e) {
    e.preventDefault();
    const MIN_WIDTH = 100;
    const MIN_HEIGHT = 100;
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
      candidatePoint2: null
    });
  }

  handleMouseLeave(e) {
    this.setState({
      candidatePoint1: null,
      candidatePoint2: null
    });
  }

  addSelection(x1, y1, x2, y2) {
    this.setState(prevState => {
      return {
        selections: [...prevState.selections, { x1, y1, x2, y2 }]
      }
    });
  }

  getRelativeMousePosition(e) {
    var rect = e.currentTarget.getBoundingClientRect();
    var x = e.pageX - rect.left - window.scrollX;
    var y = e.pageY - rect.top - window.scrollY;
    return { x, y };
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
