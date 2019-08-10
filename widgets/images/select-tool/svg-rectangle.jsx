import React from 'react';

class SvgRectangle extends React.Component {

  render() {
    return (
      <rect width={this.props.width}
            height={this.props.height}
            x={this.props.x}
            y={this.props.y}
            stroke="black"
            strokeWidth="1"
            fillOpacity="0" />
    );
  }

}

export default SvgRectangle;
