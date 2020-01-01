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

ReactDOM.render(
  <ImageGallery getImageIds={getImageIds}
                getImagesCount={getImagesCount}
                getUrlFromImageId={imageId => `/images/${imageId}`}
                pageSize={3} />,
  document.getElementById('root')
)
