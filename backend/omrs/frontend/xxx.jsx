import React from 'react'
import ReactDOM from 'react-dom'
// import SelectTool from '../../../widgets/select-tool/select-tool'
import SelectTool from 'Widgets/select-tool/select-tool'

const targetElement = document.getElementById('xxx')
const imageSrc = targetElement.getAttribute('data-base-image')

ReactDOM.render(
  <SelectTool imageSrc={imageSrc} />,
  targetElement
)
