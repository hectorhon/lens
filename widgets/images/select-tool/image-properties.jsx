import React from 'react';

function ImageProperties(props) {
  return (
    <dl className="row">
        <dt className="col-sm-3">Size</dt>
        <dd className="col-sm-9">{props.size}</dd>
        <dt className="col-sm-3">Original name</dt>
        <dd className="col-sm-9">{props.originalName}</dd>
        <dt className="col-sm-3">Upload date</dt>
        <dd className="col-sm-9">{props.uploadDate.toString()}</dd>
    </dl>
  )
}

export default ImageProperties;
