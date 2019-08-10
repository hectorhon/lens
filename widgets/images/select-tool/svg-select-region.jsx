import React from 'react';

import SvgRectangle from './svg-rectangle.jsx';

class SvgSelectRegion extends React.Component {

  render() {
    const x1 = Math.min(this.props.x1, this.props.x2);
    const y1 = Math.min(this.props.y1, this.props.y2);
    const x2 = Math.max(this.props.x1, this.props.x2);
    const y2 = Math.max(this.props.y1, this.props.y2);
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
    return (
      <>
          <SvgRectangle x={x1} y={y1} width={width} height={height} />
          {boxes.map((props, index) => (
            <SvgRectangle {...props} key={index} />
          ))}
      </>
    );
  }

}

export default SvgSelectRegion;
