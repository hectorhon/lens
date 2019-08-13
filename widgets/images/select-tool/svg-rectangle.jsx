import React from 'react';

class SvgRectangle extends React.Component {

  render() {
    const debug = this.props.debug ? (
      <>
          <text x={this.props.x} y={this.props.y}>
              ({this.props.x}, {this.props.y})
          </text>
          <text x={this.props.x + this.props.width} y={this.props.y}>
              ({this.props.x + this.props.width}, {this.props.y})
          </text>
          <text x={this.props.x} y={this.props.y + this.props.height}>
              ({this.props.x}, {this.props.y + this.props.height})
          </text>
          <text x={this.props.x + this.props.width} y={this.props.y + this.props.height}>
              ({this.props.x + this.props.width}, {this.props.y + this.props.height})
          </text>
      </>
    ) : null;
    return (
      <>
          <rect width={this.props.width}
                height={this.props.height}
                x={this.props.x}
                y={this.props.y}
                stroke={this.props.stroke}
                strokeWidth="1"
                fillOpacity="0"
                onClick={this.props.onClick} />
          {debug}
      </>
    );
  }

}


SvgRectangle.defaultProps = {
  stroke: 'black'
}

export default SvgRectangle;
