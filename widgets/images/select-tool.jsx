class SelectTool extends React.Component {

  constructor(props) {
    super(props);
    this.state = {
      point1: null,
      point2: null,
      selection: null,
    };
  }

  render() {
    return (
      <div style={{ position: "relative" }}
           onMouseDownCapture={e => this.handleMouseDown(e)}
           onMouseMoveCapture={e => this.handleMouseMove(e)}
           onMouseUpCapture={e => this.handleMouseUp(e)} >
          <svg style={{ position: "absolute", width: "100%", height: "100%" }}>
              {this.state.selection &&
               <SelectRegion x1={this.state.selection.x1}
                             y1={this.state.selection.y1}
                             x2={this.state.selection.x2}
                             y2={this.state.selection.y2} />}
              {this.state.point1 && this.state.point2 &&
               <SelectRegion x1={this.state.point1.x}
                             y1={this.state.point1.y}
                             x2={this.state.point2.x}
                             y2={this.state.point2.y} />}
          </svg>
          <img className={this.props.imgClasses}
               src={this.props.imageSrc}
               onClick={() => this.props.callback()}
          />
      </div>
    );
  }

  handleMouseDown(e) {
    e.preventDefault();
    this.setState({
      point1: this.getRelativeMousePosition(e),
      point2: this.getRelativeMousePosition(e)
    });
  }

  handleMouseMove(e) {
    e.preventDefault();
    if (this.state.point1) {
      this.setState({
        point2: this.getRelativeMousePosition(e)
      });
    }
  }

  handleMouseUp(e) {
    e.preventDefault();
    const MIN_WIDTH = 100;
    const MIN_HEIGHT = 100;
    const width = Math.abs(this.state.point1.x - this.state.point2.x);
    const height = Math.abs(this.state.point1.y - this.state.point2.y);
    if (width > MIN_WIDTH && height > MIN_HEIGHT) {
      this.setState({
        point1: null,
        point2: null,
        selection: {
          x1: this.state.point1.x,
          y1: this.state.point1.y,
          x2: this.state.point2.x,
          y2: this.state.point2.y
        }
      });
    } else {
      this.setState({
        point1: null,
        point2: null,
        selection: null
      });
    }
  }

  getRelativeMousePosition(e) {
    var rect = e.currentTarget.getBoundingClientRect();
    var x = e.pageX - rect.left - window.scrollX;
    var y = e.pageY - rect.top - window.scrollY;
    return { x, y };
  }

}

class SelectRegion extends React.Component {

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
          <Rectangle x={x1} y={y1} width={width} height={height} />
          {boxes.map((props, index) => (
            <Rectangle {...props} key={index} />
          ))}
      </>
    );
  }

}

class Rectangle extends React.Component {

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
