import React from 'react'
import ReactDOM from 'react-dom'
import ImageGallery from './image-gallery'

async function getImageIds(pageSize, pageNumber) {
  const res = await fetch(`/api/image-list?pageSize=${pageSize}&pageNumber=${pageNumber}`)
  return res.json()
}

async function deleteImageIds(imageIds, bookmarkImageId, pageSize) {
  let url = '/api/image-list/delete'
  if (bookmarkImageId && pageSize) {
    url += `?bookmark=${bookmarkImageId}&pageSize=${pageSize}`
  }
  const res = await fetch(url, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json'
    },
    body: JSON.stringify(imageIds),
  })
  return res.json()
}

ReactDOM.render(
  <ImageGallery getImageIds={getImageIds}
                getUrlFromImageId={imageId => `/images/${imageId}`}
                deleteImageIds={deleteImageIds}
                pageSize={3} />,
  document.getElementById('root')
)
