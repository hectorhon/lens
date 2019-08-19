import React from 'react';
import memoize from 'memoize-one';

import SvgContext from './svg-context.js';
import SvgSelectRegionHandle from './svg-select-region-handle.jsx';
import SvgRectangle from './svg-rectangle.jsx';

class SvgSelectRegion extends React.Component {

  static contextType = SvgContext;

  constructor(props) {
    super(props);
  }

  render() {
    const { x1, y1, x2, y2, width, height, boxes } =
      this.computeDerivedPropsMemoized(this.props.x1, this.props.y1, this.props.x2, this.props.y2);

    const frame = (
      <SvgRectangle x={x1} y={y1} width={width} height={height}
                    stroke={this.props.highlight ? "green" : "black" } />
    );

    return (
      <g {...(this.props.isCandidate ? { } : { onMouseEnter: this.handleMouseEnter })} >
          {frame}
          {boxes.map((props, index) => (
            <SvgRectangle {...props} key={index} />
          ))}
          <SvgSelectRegionHandle x={this.props.x1} y={this.props.y1}
                                 onDragStart={this.props.onDragStart}
                                 onNewPosition={this.updateTopLeft}
                                 onDragStop={this.props.onDragStop} />
          <SvgSelectRegionHandle x={this.props.x2} y={this.props.y2}
                                 onDragStart={this.props.onDragStart}
                                 onNewPosition={this.updateBottomRight}
                                 onDragStop={this.props.onDragStop} />
      </g>
    );
  }

  computeDerivedProps(propsX1, propsY1, propsX2, propsY2) {
    const x1 = Math.min(propsX1, propsX2);
    const y1 = Math.min(propsY1, propsY2);
    const x2 = Math.max(propsX1, propsX2);
    const y2 = Math.max(propsY1, propsY2);
    const width = x2 - x1;
    const height = y2 - y1;
    const rows = 40;
    const columns = 5;
    const spacing = 2;
    const boxWidth = (width - spacing) / columns - spacing;
    const boxHeight = (height - spacing) / rows - spacing;
    var boxes = [];
    if (boxWidth > spacing + 4 && boxHeight > spacing + 4) {
      for (let row = 0; row < rows; row++) {
        for (let col = 0; col < columns; col++) {
          boxes.push({
            x: x1 + spacing + col * (boxWidth + spacing),
            y: y1 + spacing + row * (boxHeight + spacing),
            width: boxWidth,
            height: boxHeight
          });
        }
      }
    }
    return { x1, y1, x2, y2, width, height, boxes };
  }

  computeDerivedPropsMemoized = memoize(this.computeDerivedProps);

  updateTopLeft = (x1, y1) => {
    this.props.onRegionCoordsChanged &&
    this.props.onRegionCoordsChanged(this.props.id, {
      x1, y1,
      x2: this.props.x2,
      y2: this.props.y2,
    });
  }

  updateBottomRight = (x2, y2) => {
    this.props.onRegionCoordsChanged &&
    this.props.onRegionCoordsChanged(this.props.id, {
      x1: this.props.x1,
      y1: this.props.y1,
      x2, y2
    });
  }

  handleMouseEnter = e => {
    this.props.onMouseEnter &&
    this.props.onMouseEnter(this.props.id);
  }

}

export default SvgSelectRegion;
