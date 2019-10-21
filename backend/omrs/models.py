from django.db import models
from django.urls import reverse
from uuid import uuid4

import json

from core.models import Image, Album


class Template(models.Model):
    id = models.UUIDField(primary_key=True, default=uuid4, editable=False)
    name = models.CharField(max_length=200)
    created_on = models.DateTimeField(auto_now_add=True)
    updated_on = models.DateTimeField(auto_now=True)
    base_image = models.ForeignKey(Image, on_delete=models.PROTECT)

    def __str__(self):
        return '%s (Last updated %s)' % (self.name, self.updated_on.strftime('%-d %b %Y'))

    def get_absolute_url(self):
        return reverse('omrs:template_view', args=(self.id,))

    def parse_selections(json_string, template_id):
        """Convert selections from front end json model to Django model."""
        selections = json.loads(json_string)
        return [Selection(
            id=selection['id'],
            template_id=template_id,
            order=index,
            name=selection['name'],
            x=selection['x'],
            y=selection['y'],
            width=selection['width'],
            height=selection['height'],
            num_rows=selection['numRows'],
            num_columns=selection['numColumns'],
            spacing_x=selection['spacingX'],
            spacing_y=selection['spacingY'],
        ) for index, selection in enumerate(selections, start=1)]

    def selections_to_json(selections):
        """Convert selections from Django model to front end json model."""
        return json.dumps([{
            'id': str(selection.id),
            'name': selection.name,
            'x': selection.x,
            'y': selection.y,
            'width': selection.width,
            'height': selection.height,
            'numRows': selection.num_rows,
            'numColumns': selection.num_columns,
            'spacingX': selection.spacing_x,
            'spacingY': selection.spacing_y,
        } for selection in selections.order_by('order')])


class Selection(models.Model):
    id = models.UUIDField(primary_key=True, editable=False)
    template = models.ForeignKey(Template, on_delete=models.CASCADE)
    order = models.IntegerField(default=1)
    name = models.CharField(max_length=200)
    x = models.IntegerField(default=0)
    y = models.IntegerField(default=0)
    width = models.IntegerField(default=0)
    height = models.IntegerField(default=0)
    num_rows = models.IntegerField(default=0)
    num_columns = models.IntegerField(default=0)
    spacing_x = models.IntegerField(default=0)
    spacing_y = models.IntegerField(default=0)

    class Meta:
        constraints = [
            models.UniqueConstraint(fields=['template', 'order'], name='unique_ordering')
        ]


class Operation(models.Model):
    id = models.UUIDField(primary_key=True, default=uuid4, editable=False)
    created_on = models.DateTimeField(auto_now_add=True)
    template = models.ForeignKey(Template, on_delete=models.PROTECT)
    album = models.ForeignKey(Album, on_delete=models.PROTECT)

    def get_absolute_url(self):
        return reverse('omrs:operation_view', args=(self.id,))
