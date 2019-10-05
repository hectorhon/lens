function render() {
  const numSelected = $('.album-image.selected').length
  if (numSelected === 1) {
    $('#selected-count-text').text('1 image selected.')
  } else {
    $('#selected-count-text').text(`${numSelected} images selected.`)
  }
  if (numSelected) {
    $('#images-toolbar').css('visibility', 'visible')
  } else {
    $('#images-toolbar').css('visibility', 'hidden')
  }
}

$('.album-image').click(function() {
  $(this).toggleClass('selected')
  render()
})

$('#delete-selected-images-button').click(() => {
  const selectedImageIds = $('.album-image.selected').map(function() {
    return $(this).data('imageId')
  }).get()
  if (confirm('Are you sure you want to delete the selected images?')) {
    const csrfToken = $('[name=csrfmiddlewaretoken]').val()
    $.ajax({
      type: 'POST',
      url: '/core/images/api/delete',
      headers: {
        'X-CSRFToken': csrfToken
      },
      data: { selectedImageIds },
      success: () => {
        location.reload(true)
      },
    })
  }
})

render()
