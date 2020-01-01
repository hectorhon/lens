import React from 'react'
import PropTypes from 'prop-types'

import ImageGalleryItem from './image-gallery-item'

class ImageGallery extends React.Component {
  constructor(props) {
    super(props)
    this.state = {
      pageNumber: undefined,
      imageIds: [],
      totalPages: 1,
      selectedImageIds: [],
    }
  }

  async componentDidMount() {
    await this.navigateToPage(1)
  }

  async navigateToPage(newPageNumber) {
    const { pageSize, getImageIds, getImagesCount } = this.props
    const { pageNumber, imageIds } = this.state

    let getImageIdsPromise
    if (newPageNumber > pageNumber) {
      const fromId = imageIds[pageSize - 1]
      getImageIdsPromise = getImageIds(fromId, pageSize)
    } else if (newPageNumber < pageNumber) {
      const fromId = imageIds[0]
      getImageIdsPromise = getImageIds(fromId, -pageSize)
    } else if (pageNumber === undefined) {
      const fromId = undefined
      getImageIdsPromise = getImageIds(fromId, pageSize)
    }

    const [newImageIds, imagesCount] = await Promise.all([getImageIdsPromise, getImagesCount()])
    const totalPages = Math.ceil(imagesCount / pageSize)

    this.setState({
      imageIds: newImageIds,
      totalPages,
      pageNumber: newPageNumber,
    })
  }

  async refreshCurrentPage() {
    const { pageNumber: currentPageNumber } = this.state
    this.setState({
      pageNumber: undefined
    })
    await this.navigateToPage(currentPageNumber)
  }

  toggleSelectImage(imageId) {
    const { selectedImageIds } = this.state
    const index = selectedImageIds.indexOf(imageId)
    if (index === -1) {
      this.setState({
        selectedImageIds: [...selectedImageIds, imageId]
      })
    } else {
      const newSelectedImageIds = selectedImageIds.slice()
      newSelectedImageIds.splice(index, 1)
      this.setState({
        selectedImageIds: newSelectedImageIds
      })
    }
  }

  async deleteSelectedImages() {
    const { deleteImages } = this.props
    const { selectedImageIds } = this.state
    await deleteImages(selectedImageIds)
    this.refreshCurrentPage()
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
        <div className="ImageGalleryToolbar">
          {selectedImageIds.length > 0 && (
            <div>
              <button type="button" onClick={() => this.deleteSelectedImages()}>Delete</button>
            </div>
          )}
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
  getImagesCount: PropTypes.func.isRequired,
  getUrlFromImageId: PropTypes.func.isRequired,
  deleteImages: PropTypes.func.isRequired,
  pageSize: PropTypes.number,
}

ImageGallery.defaultProps = {
  pageSize: 8,
}

export default ImageGallery
