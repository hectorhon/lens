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
    var x = e.pageX - rect.left;
    var y = e.pageY - rect.top;
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
    const spacing = 4;
    const boxWidth = Math.floor((width - spacing) / columns - spacing);
    const boxHeight = Math.floor((height - spacing) / rows - spacing);
    console.log(boxWidth, boxHeight);
    var boxes = [];
    if (boxWidth > spacing + 4 && boxHeight > spacing + 4) {
      for (let row = 0; row < rows; row++) {
        for (let col = 0; col < columns; col++) {
          boxes.push({
            x: spacing + col * (boxWidth + spacing),
            y: spacing + row * (boxHeight + spacing),
          });
        }
      }
    }
    return (
      <Rectangle x={x1} y={y1} width={width} height={height}>
          {boxes.map(({ x, y }, index) => (
            <Rectangle x={x} y={y} width={boxWidth} height={boxHeight}
                       key={index} />
          ))}
      </Rectangle>
    );
  }

}

class Rectangle extends React.Component {

  render() {
    return (
      <div style={{
        position: "absolute",
        left: this.props.x + "px",
        top: this.props.y + "px",
        width: this.props.width + "px",
        height: this.props.height + "px",
        border: "1px solid black",
      }}>
          {this.props.children}
      </div>
    );
  }

}

class Rectangle0 extends React.Component {

  render() {
    const x1 = Math.min(this.props.x1, this.props.x2);
    const y1 = Math.min(this.props.y1, this.props.y2);
    const x2 = Math.max(this.props.x1, this.props.x2);
    const y2 = Math.max(this.props.y1, this.props.y2);
    const width = x2 - x1;
    const height = y2 - y1;
    return (
      <div style={{
        position: "absolute",
        left: x1 + "px",
        top: y1 + "px",
        width: width + "px",
        height: height + "px",
        border: "1px solid black",
      }}>
      </div>
    );
  }

}
