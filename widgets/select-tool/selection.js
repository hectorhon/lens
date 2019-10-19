import uuid from 'uuid'

function ensureBetween(min, n, max) {
  if (n < min) {
    return min
  }
  if (n > max) {
    return max
  }
  return n
}

class Selection {
  static create(x1, y1, x2, y2, name) {
    const selection = new Selection()
    selection.id = uuid.v4()
    selection.name = name
    selection.x = Math.min(x1, x2)
    selection.y = Math.min(y1, y2)
    selection.width = Math.abs(x1 - x2)
    selection.height = Math.abs(y1 - y2)
    selection.numRows = 20
    selection.numColumns = 4
    selection.spacingX = 4
    selection.spacingY = 4
    return selection
  }

  static fromJson(object) {
    const selection = new Selection()
    selection.id = object.id
    selection.order = object.order
    selection.name = object.name
    selection.x = object.x
    selection.y = object.y
    selection.width = object.width
    selection.height = object.height
    selection.numRows = object.numRows
    selection.numColumns = object.numColumns
    selection.spacingX = object.spacingX
    selection.spacingY = object.spacingY
    return selection
  }

  getGridWidth() {
    return (this.width - (this.numColumns - 1) * this.spacingX) / this.numColumns
  }

  getGridHeight() {
    return (this.height - (this.numRows - 1) * this.spacingY) / this.numRows
  }

  setSpacingX(n) {
    if (n < this.MIN_SPACING_X) {
      return
    }
    const originalSpacingX = this.spacingX
    this.spacingX = n
    const newGridWidth = this.getGridWidth()
    if (newGridWidth < this.MIN_GRID_WIDTH) {
      this.spacingX = originalSpacingX
    }
  }

  setSpacingY(n) {
    if (n < this.MIN_SPACING_Y) {
      return
    }
    const originalSpacingY = this.spacingY
    this.spacingY = n
    const newGridHeight = this.getGridHeight()
    if (newGridHeight < this.MIN_GRID_HEIGHT) {
      this.spacingY = originalSpacingY
    }
  }

  setNumRows(n) {
    this.numRows = ensureBetween(this.MIN_NUM_ROWS, n, this.MAX_NUM_ROWS)
  }

  setNumColumns(n) {
    this.numColumns = ensureBetween(this.MIN_NUM_COLUMNS, n, this.MAX_NUM_COLUMNS)
  }

  ensurePositive() {
    if (this.width < 0) {
      this.x += this.width
      this.width = Math.abs(this.width)
    }
    if (this.height < 0) {
      this.y += this.height
      this.height = Math.abs(this.height)
    }
  }

  updateUpperLeft(x, y) {
    const xDiff = x - this.x
    this.x += xDiff
    this.width -= xDiff

    const yDiff = y - this.y
    this.y += yDiff
    this.height -= yDiff
  }

  updateLowerRight(x, y) {
    this.width = x - this.x
    this.height = y - this.y
  }
}

Selection.prototype.MIN_NUM_ROWS = 5
Selection.prototype.MAX_NUM_ROWS = 50
Selection.prototype.MIN_NUM_COLUMNS = 3
Selection.prototype.MAX_NUM_COLUMNS = 6
Selection.prototype.MIN_SPACING_X = 4
Selection.prototype.MIN_SPACING_Y = 4
Selection.prototype.MIN_GRID_WIDTH = 16
Selection.prototype.MIN_GRID_HEIGHT = 16

export default Selection
