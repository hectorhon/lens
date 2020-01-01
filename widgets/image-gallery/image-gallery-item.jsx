import React from 'react'
import PropTypes from 'prop-types'

class ImageGalleryItem extends React.Component {
  render() {
    const { url } = this.props
    return (
      <img src={url}
           alt=""
           width="300px"
           height="300px"
           style={{ objectFit: 'contain' }} />
    )
  }
}

ImageGalleryItem.propTypes = {
  url: PropTypes.string.isRequired,
}

export default ImageGalleryItem
