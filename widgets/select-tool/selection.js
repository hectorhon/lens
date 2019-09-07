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
  constructor(x1, y1, x2, y2) {
    this.id = uuid.v4()
    // this.x1 = x1
    // this.y1 = y1
    // this.x2 = x2
    // this.y2 = y2
    this.x = Math.min(x1, x2)
    this.y = Math.min(y1, y2)
    this.width = Math.abs(x1 - x2)
    this.height = Math.abs(y1 - y2)
    this.numRows = 20
    this.numColumns = 4
    this.MIN_NUM_ROWS = 5
    this.MAX_NUM_ROWS = 50
    this.MIN_NUM_COLUMNS = 3
    this.MAX_NUM_COLUMNS = 6
    this.spacingX = 4
    this.spacingY = 4
    this.MIN_SPACING_X = 4
    this.MIN_SPACING_Y = 4
    this.MIN_GRID_WIDTH = 16
    this.MIN_GRID_HEIGHT = 16
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

export default Selection
