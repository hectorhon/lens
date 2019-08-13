import React from 'react';

class SelectRegionProperties extends React.Component {

  constructor(props) {
    super(props);
    this.state = {
      userChoice: "", // id of a select region
    };
    this.handleUserChoice = this.handleUserChoice.bind(this);
  }

  render() {

    const selectRegion = this.props.selections.find(
      selection => selection.id === this.state.userChoice);

    const properties = selectRegion && (
      <p>{selectRegion.x1}</p>
    );

    return (
      <div className="row">
          <select value={this.state.userChoice} onChange={this.handleUserChoice}>
              {this.props.selections.map(selection => {
                return (
                  <option key={selection.id} value={selection.id}>{selection.id}</option>
                );
              })}
          </select>
          {properties}
      </div>
    );
  }

  handleUserChoice(e) {
    this.setState({
      userChoice: e.target.value
    });
  }

}

export default SelectRegionProperties;
