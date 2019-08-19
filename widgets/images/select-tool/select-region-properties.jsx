import React from 'react';

class SelectRegionProperties extends React.Component {

  constructor(props) {
    super(props);
    this.handleInputChange = this.handleInputChange.bind(this);
    this.handleKeyDown = this.handleKeyDown.bind(this);
  }

  render() {
    const fields = ["x1", "y1", "x2", "y2"].map(fieldName => (
      <React.Fragment key={fieldName}>
          <label htmlFor={fieldName}>{fieldName}</label>
          <input className="form-control" id={fieldName} name={fieldName}
                 value={this.props.selection[fieldName]}
                 onChange={this.handleInputChange}
                 onKeyDown={this.handleKeyDown} />
      </React.Fragment>
    ));
    return (
      <form>
          <div className="form-group">
              {fields}
          </div>
      </form>
    );
  }

  handleInputChange(e) {
    // this.props.modifySelection(
    //   this.props.selection.id,
    //   { [e.target.name]: e.target.value }
    // );
  }

  handleKeyDown(e) {
    const originalValue = this.props.selection[e.target.name];
    if (e.keyCode == 38) { // arrow up
      var newValue = originalValue + 1;
    } else if (e.keyCode == 40) { // arrow down
      var newValue = originalValue - 1;
    } else {
      return;
    }
    this.props.modifySelection(
      this.props.selection.id,
      { [e.target.name]: newValue }
    );
  }

}

export default SelectRegionProperties;
