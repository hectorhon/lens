import uuid from 'uuid'

class Selection {
  constructor(x1, y1, x2, y2) {
    this.id = uuid.v4()
    this.x1 = x1
    this.y1 = y1
    this.x2 = x2
    this.y2 = y2
    this.x = Math.min(x1, x2)
    this.y = Math.min(y1, y2)
    this.width = Math.abs(x1 - x2)
    this.height = Math.abs(y1 - y2)
  }
}

export default Selection
