from celery import shared_task
from django.core import serializers

from PIL import Image
import math
import random


@shared_task(bind=True)
def scan_task(self, serialized_selections, image_filepaths):
    print('Request: {0!r}'.format(self.request))

    selections = [deserialized_object.object
                  for deserialized_object in serializers.deserialize('json', serialized_selections)]

    page = Page(selections)

    for image_filepath in image_filepaths:
        image = Image.open(image_filepath)
        page.set_image(image)
        choices = page.get_choices()
        print(image_filepath, choices)


class Cell:
    def __init__(self, x, y, width, height):
        self.x = x
        self.y = y
        self.width = width
        self.height = height
        self.image = None
        self.value = None

    def __str__(self):
        return 'Cell (%d, %d)' % (self.x, self.y)

    __repr__ = __str__

    def extract_image_part(self, whole_image):
        # box is (left coords, upper coords, right coords, lower coords)
        box = (self.x, self.y, self.x + self.width, self.y + self.height)
        self.image = whole_image.crop(box)

    def estimate_value(self):
        self.value = random.random()


class Row:
    def __init__(self, number, num_columns,
                 x, y, width, height, spacing_x):
        self.number = number  # 1 based
        self.num_columns = num_columns
        self.x = x
        self.y = y
        self.width = width
        self.height = height
        self.spacing_x = spacing_x

        cell_width = math.ceil((width - spacing_x * (num_columns - 1)) / num_columns)
        cell_height = height
        self.cells = [
            Cell(x + (cell_width + spacing_x) * column_index,
                 y,
                 cell_width,
                 cell_height)
            for column_index in range(num_columns)
        ]

        self.choice = None

    def determine_choice(self):
        for cell in self.cells:
            cell.estimate_value()

        values = [cell.value for cell in self.cells]
        choice = values.index(max(values))
        self.choice = choice


class Page:
    def __init__(self, selections):
        self.rows = []
        for selection in selections:
            row_width = selection.width
            row_height = math.ceil(
                (selection.height - selection.spacing_y * (selection.num_rows - 1))
                / selection.num_rows
            )
            for row_index in range(selection.num_rows):
                row = Row(row_index + 1,  # 1 based
                          selection.num_columns,
                          selection.x,
                          selection.y + (row_height + selection.spacing_y) * row_index,
                          row_width,
                          row_height,
                          selection.spacing_x)
                self.rows.append(row)

        self.current_image = None

    def set_image(self, image):
        self.current_image = image
        for row in self.rows:
            for cell in row.cells:
                cell.extract_image_part(self.current_image)

    def dump_cells(self):
        for row in self.rows:
            for cell_index, cell in enumerate(row.cells):
                filename = 'img_%03d_%d.jpg' % (row.number, cell_index)
                cell.image.save(filename)

    def get_choices(self):
        for row in self.rows:
            row.determine_choice()

        return [row.choice for row in self.rows]
