import React from 'react'
import ReactDOM from 'react-dom'
import ImageGallery from './image-gallery'

async function getImageIds(fromId, count) {
  const res = await fetch(`/api/image-list?fromId=${fromId}&count=${count}`)
  return res.json()
}

async function getImagesCount() {
  const res = await fetch('/api/image-list-count')
  return res.json()
}

async function deleteImages(imageIds) {
  const res = await fetch('/api/image-list-delete', {
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
                getImagesCount={getImagesCount}
                getUrlFromImageId={imageId => `/images/${imageId}`}
                deleteImages={deleteImages}
                pageSize={8} />,
  document.getElementById('root')
)
