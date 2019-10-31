import React from 'react'
import ReactDOM from 'react-dom'

class Row extends React.Component {
  render() {
    const { values } = this.props
    const cells = values.map(value => (
      <td>
        {value}
      </td>
    ))
    return (
      <tr>
        {cells}
      </tr>
    )
  }
}

class Table extends React.Component {
  render() {
    const { data } = this.props
    const rows = data.map(entry => (
      <Row values={entry} />
    ))
    return (
      <table>
        <thead>
        </thead>
        <tbody>
          {rows}
        </tbody>
      </table>
    )
  }
}

const data = [{
  name: 'asdf',
  age: 12
}, {
  name: 'qwer',
  age: 34
}]

ReactDOM.render(
  <Table data={data} columnNames={['name', 'age']} />,
  document.getElementById('root')
)
