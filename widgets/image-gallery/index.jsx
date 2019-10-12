import React from 'react'
import ReactDOM from 'react-dom'
import ImageGallery from './image-gallery'

async function getImageIds(pageSize, pageNumber) {
  const res = await fetch(`/api/image-list?pageSize=${pageSize}&pageNumber=${pageNumber}`)
  return res.json()
}

ReactDOM.render(
  <ImageGallery getImageIds={getImageIds}
                getUrlFromImageId={imageId => `/images/${imageId}`}
                pageSize={3} />,
  document.getElementById('root')
)
