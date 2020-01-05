import React from 'react'
import PropTypes from 'prop-types'

class ImageGalleryItem extends React.Component {
  render() {
    const { url, isHighlighted, onClick } = this.props
    return (
      <div className={'ImageGalleryItem-Border ' + (isHighlighted && 'ImageGalleryItem-Border_highlighted')}
           onClick={onClick}>
        <img src={url}
             alt=""
             width="300px"
             height="300px"
             style={{ objectFit: 'contain' }} />
      </div>
    )
  }
}

ImageGalleryItem.propTypes = {
  url: PropTypes.string.isRequired,
  isHighlighted: PropTypes.bool,
  onClick: PropTypes.func,
}

ImageGalleryItem.defaultProps = {
  isHighlighted: false,
  onClick: () => {}
}

export default ImageGalleryItem
