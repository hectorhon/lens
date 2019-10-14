import React from 'react'
import ReactDOM from 'react-dom'
// import SelectTool from '../../../widgets/select-tool/select-tool'
import SelectTool from 'Widgets/select-tool/select-tool'

const targetElement = document.getElementById('xxx')
const templateId = targetElement.getAttribute('data-template-id')
const imageSrc = targetElement.getAttribute('data-base-image')

function getCookie(name) {
  let cookieValue = null
  if (document.cookie && document.cookie !== '') {
    const cookies = document.cookie.split(';')
    for (let i = 0; i < cookies.length; i += 1) {
      const cookie = cookies[i].trim()
      if (cookie.substring(0, name.length + 1) === `${name}=`) {
        cookieValue = decodeURIComponent(cookie.substring(name.length + 1))
        break
      }
    }
  }
  return cookieValue
}
const csrfToken = getCookie('csrftoken')

function save(jsonState) {
  const data = { templateId, ...jsonState }
  fetch('/omrs/templates/api/update_template_selections/', {
    method: 'post',
    headers: { 'X-CSRFToken': csrfToken },
    credentials: 'include',
    body: JSON.stringify(data)
  }).then(response => {
    if (!response.ok) {
      alert('An error occurred while saving the selections.')
    }
    window.location.reload()
  })
}

ReactDOM.render(
  <SelectTool imageSrc={imageSrc} save={save} />,
  targetElement
)
