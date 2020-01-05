import React from 'react'
import PropTypes from 'prop-types'

import ImageGalleryItem from './image-gallery-item'

class ImageGallery extends React.Component {
  constructor(props) {
    super(props)
    this.state = {
      pageNumber: 1,
      imageIds: [],
      totalPages: 1,
      selectedImageIds: [],
    }
  }

  async componentDidMount() {
    const { pageNumber } = this.state
    await this.navigateToPage(pageNumber)
  }

  async navigateToPage(pageNumber) {
    const { pageSize, getImageIds } = this.props
    const { imageIds, totalPages } = await getImageIds(pageSize, pageNumber)
    this.setState({
      imageIds, totalPages, pageNumber
    })
  }

  toggleSelectImage(imageId) {
    const { selectedImageIds } = this.state
    let newSelectedImageIds = selectedImageIds.slice()
    if (selectedImageIds.includes(imageId)) {
      newSelectedImageIds = newSelectedImageIds.filter(id => id !== imageId)
    } else {
      newSelectedImageIds.push(imageId)
    }
    this.setState({
      selectedImageIds: newSelectedImageIds
    })
  }

  async deleteSelectedImages() {
    const { deleteImageIds } = this.props
    const { selectedImageIds } = this.state
    await deleteImageIds(selectedImageIds)
  }

  render() {
    const { getUrlFromImageId } = this.props
    const {
      pageNumber, imageIds, totalPages, selectedImageIds
    } = this.state

    const prevPageButton = (
      <button type="button" onClick={() => this.navigateToPage(pageNumber - 1)}>Previous</button>
    )
    const nextPageButton = (
      <button type="button" onClick={() => this.navigateToPage(pageNumber + 1)}>Next</button>
    )
    return (
      <>
        <h1>Hello</h1>
        <div>
          <button type="button"
                  disabled={selectedImageIds.length === 0}
                  onClick={() => this.deleteSelectedImages()}>
            Delete
          </button>
        </div>
        {imageIds.map(imageId => {
          const url = getUrlFromImageId(imageId)
          return (
            <ImageGalleryItem key={imageId}
                              url={url}
                              isHighlighted={selectedImageIds.includes(imageId)}
                              onClick={() => this.toggleSelectImage(imageId)} />
          )
        })}
        <div className="pagination">
          {pageNumber > 1 && prevPageButton}
          <span>Page {pageNumber}</span>
          {pageNumber < totalPages && nextPageButton}
        </div>
      </>
    )
  }
}

ImageGallery.propTypes = {
  getImageIds: PropTypes.func.isRequired,
  getUrlFromImageId: PropTypes.func.isRequired,
  deleteImageIds: PropTypes.func.isRequired,
  pageSize: PropTypes.number,
}

ImageGallery.defaultProps = {
  pageSize: 8,
}

export default ImageGallery
