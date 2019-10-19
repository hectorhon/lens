import React from 'react'
import ReactDOM from 'react-dom'
// import SelectTool from '../../../widgets/select-tool/select-tool'
import SelectTool from 'Widgets/select-tool/select-tool'

const targetElement = document.getElementById('xxx')
const imageSrc = targetElement.getAttribute('data-base-image')
const form = document.getElementById('template-selections-form')

function save(jsonState) {
  const str = JSON.stringify(jsonState)
  form.selections.value = str
  form.submit()
}

const initialSelections = JSON.parse(
  document.getElementById('template-selections-form').selections.value
)

ReactDOM.render(
  <SelectTool imageSrc={imageSrc} save={save} initialSelections={initialSelections} />,
  targetElement
)
