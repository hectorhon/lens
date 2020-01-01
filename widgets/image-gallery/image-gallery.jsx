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
    }
  }

  async componentDidMount() {
    await this.navigateToPage(1)
  }

  async navigateToPage(newPageNumber) {
    const { pageSize, getImageIds, getImagesCount } = this.props
    const { pageNumber, imageIds } = this.state

    let getImageIdsPromise
    if (newPageNumber > pageNumber || pageNumber === undefined) {
      const fromId = imageIds[pageSize - 1]
      getImageIdsPromise = getImageIds(fromId, pageSize)
    } else {
      const fromId = imageIds[0]
      getImageIdsPromise = getImageIds(fromId, -pageSize)
    }

    const [newImageIds, imagesCount] = await Promise.all([getImageIdsPromise, getImagesCount()])
    const totalPages = Math.ceil(imagesCount / pageSize)

    this.setState({
      imageIds: newImageIds,
      totalPages,
      pageNumber: newPageNumber,
    })
  }

  render() {
    const { getUrlFromImageId } = this.props
    const { pageNumber, imageIds, totalPages } = this.state

    const prevPageButton = (
      <button type="button" onClick={() => this.navigateToPage(pageNumber - 1)}>Previous</button>
    )
    const nextPageButton = (
      <button type="button" onClick={() => this.navigateToPage(pageNumber + 1)}>Next</button>
    )
    return (
      <>
        <h1>Hello</h1>
        {imageIds.map(imageId => {
          const url = getUrlFromImageId(imageId)
          return (
            <ImageGalleryItem key={imageId} url={url} />
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
  pageSize: PropTypes.number,
}

ImageGallery.defaultProps = {
  pageSize: 8,
}

export default ImageGallery
