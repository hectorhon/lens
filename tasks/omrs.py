import zipfile
import json
import io
from PIL import Image


class Cell:
    def __init__(self, x, y, width, height):
        self.x = x
        self.y = y
        self.width = width
        self.height = height

    def __str__(self):
        return 'Cell (%d, %d)' % (self.x, self.y)

    __repr__ = __str__

    def to_crop_region_tuple(self):
        """(left, upper, right, lower)"""
        return (self.x, self.y, self.x + self.width, self.y + self.height)


class Selection:
    def __init__(self, json_dict):
        self.x = json_dict['x']
        self.y = json_dict['y']
        self.width = json_dict['width']
        self.height = json_dict['height']
        self.num_rows = json_dict['numRows']
        self.num_columns = json_dict['numColumns']
        self.spacing_x = json_dict['spacingX']
        self.spacing_y = json_dict['spacingY']

    def __str__(self):
        return 'Selection (%d, %d) (%d x %d)' \
            % (self.x, self.y, self.num_rows, self.num_columns)

    __repr__ = __str__

    def get_cell_groups(self):
        cell_width = self.width - self.spacing_x * (self.num_columns + 1)
        cell_height = self.height - self.spacing_y * (self.num_rows + 1)
        cell_groups = []

        for row_index in range(self.num_rows):

            cell_y = (self.y + self.spacing_y) + (row_index * (cell_height + self.spacing_y))
            cells = []

            for col_index in range(self.num_columns):
                cell_x = (self.x + self.spacing_x) + (col_index * (cell_width + self.spacing_x))
                cells.append(Cell(cell_x, cell_y, cell_width, cell_height))

            cell_groups.append(cells)

        return cell_groups


with open('test.zip', 'rb') as bytes:
    with zipfile.ZipFile(bytes) as zip:
        # TODO: scaling
        selections = [Selection(dict) for dict in json.loads(zip.read('selections.json'))]
        images = [Image.open(io.BytesIO(zip.read(zi)))
                  for zi in zip.infolist() if zi.filename != 'selections.json']
        for image in images:
            for selection_index, selection in enumerate(selections, start=1):
                for cell_group_index, cell_group in enumerate(selection.get_cell_groups(), start=1):
                    for cell_index, cell in enumerate(cell_group, start=1):
                        filename = 'img_%03d_%03d_%d.jpg' % \
                            (selection_index, cell_group_index, cell_index)
                        box = cell.to_crop_region_tuple()
                        region = image.crop(box)
                        region.save(filename)
                break
