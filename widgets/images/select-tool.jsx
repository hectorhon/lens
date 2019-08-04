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
           <Rectangle x1={this.state.selection.x1}
                      y1={this.state.selection.y1}
                      x2={this.state.selection.x2}
                      y2={this.state.selection.y2} />}
          {this.state.point1 && this.state.point2 &&
           <Rectangle x1={this.state.point1.x}
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
  }

  getRelativeMousePosition(e) {
    var rect = e.currentTarget.getBoundingClientRect();
    var x = e.pageX - rect.left;
    var y = e.pageY - rect.top;
    return { x, y };
  }

}

class Rectangle extends React.Component {

  render() {
    const _x1 = Math.min(this.props.x1, this.props.x2);
    const _y1 = Math.min(this.props.y1, this.props.y2);
    const _x2 = Math.max(this.props.x1, this.props.x2);
    const _y2 = Math.max(this.props.y1, this.props.y2);
    return (
      <div style={{
        position: "absolute",
        left: _x1 + "px",
        top: _y1 + "px",
        width: (_x2 - _x1) + "px",
        height: (_y2 - _y1) + "px",
        border: "1px solid black",
      }}></div>
    );
  }

}
