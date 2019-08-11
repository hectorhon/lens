import React from 'react';

import SvgContext from './svg-context.js';

class SvgSelectRegionHandle extends React.Component {

  static contextType = SvgContext;

  constructor(props) {
    super(props);
    this.state = {
      dragging: false,
      offsetX: null, // offset from mouse down position to handle center
      offsetY: null,
    };
  }

  render() {
    return (
      <circle cx={this.props.x} cy={this.props.y} r={16}
              onMouseDown={this.handleMouseDown}
              onMouseMove={this.handleMouseMove}
              onMouseUp={this.handleMouseUp}
              onMouseLeave={this.handleMouseLeave} />
    );
  }

  handleMouseDown = e => {
    e.stopPropagation();
    e.preventDefault();
    const { x, y } = this.context.getRelativeMousePosition(e);
    this.setState({
      dragging: true,
      offsetX: x - this.props.x,
      offsetY: y - this.props.y
    });
  }

  handleMouseMove = e => {
    if (this.state.dragging) {
      const { x, y } = this.context.getRelativeMousePosition(e);
      this.props.onNewPosition(x - this.state.offsetX, y - this.state.offsetY);
    }
  }

  handleMouseUp = e => {
    e.stopPropagation();
    e.preventDefault();
    this.setState({
      dragging: false,
      offsetX: null,
      offsetY: null
    });
  }

  handleMouseLeave = e => {
    this.setState({
      dragging: false,
      offsetX: null,
      offsetY: null
    });
  }

}

export default SvgSelectRegionHandle;
