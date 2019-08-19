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
      <circle cx={this.props.x} cy={this.props.y} r={4}
              fill="white" stroke="black" strokeWidth={1}
              onMouseDown={this.handleMouseDown}
              onMouseUp={this.handleMouseUp} />
    );
  }

  componentWillUnmount() {
    this.context.svgRef.current.removeEventListener('mousemove', this.handleMouseMove);
    this.context.svgRef.current.removeEventListener('mouseup', this.handleMouseUp);
  }

  handleMouseDown = e => {
    e.stopPropagation(); // needed to avoid creating new region
    e.preventDefault();  // needed to avoid triggering drag
    const { x, y } = this.context.getRelativeMousePosition(e);
    this.setState({
      dragging: true,
      offsetX: x - this.props.x,
      offsetY: y - this.props.y
    });
    this.props.onDragStart && this.props.onDragStart();
    this.context.svgRef.current.addEventListener('mousemove', this.handleMouseMove);
    this.context.svgRef.current.addEventListener('mouseup', this.handleMouseUp);
  }

  handleMouseMove = e => {
    if (this.state.dragging) {
      const { x, y } = this.context.getRelativeMousePosition(e);
      this.props.onNewPosition(x - this.state.offsetX, y - this.state.offsetY);
    }
  }

  handleMouseUp = e => {
    this.setState({
      dragging: false,
      offsetX: null,
      offsetY: null
    });
    this.props.onDragStop && this.props.onDragStop();
    this.context.svgRef.current.removeEventListener('mousemove', this.handleMouseMove);
    this.context.svgRef.current.removeEventListener('mouseup', this.handleMouseUp);
  }

}

export default SvgSelectRegionHandle;
