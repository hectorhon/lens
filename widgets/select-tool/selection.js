import uuid from 'uuid'

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
