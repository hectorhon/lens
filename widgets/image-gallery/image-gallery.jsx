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
            <ImageGalleryItem key={imageId}
                              url={url} />
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
  pageSize: PropTypes.number,
}

ImageGallery.defaultProps = {
  pageSize: 8,
}

export default ImageGallery
