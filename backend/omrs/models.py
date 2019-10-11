from django.db import models
from django.urls import reverse
from uuid import uuid4

from core.models import Image


class Template(models.Model):
    id = models.UUIDField(primary_key=True, default=uuid4, editable=False)
    name = models.CharField(max_length=200)
    created_on = models.DateTimeField(auto_now_add=True)
    base_image = models.ForeignKey(Image, on_delete=models.PROTECT)

    def get_absolute_url(self):
        return reverse('omrs:template_view', args=(self.id,))


class Selection(models.Model):
    id = models.UUIDField(primary_key=True, editable=False)
    template = models.ForeignKey(Template, on_delete=models.CASCADE)
    name = models.CharField(max_length=200)
    x = models.IntegerField(default=0)
    y = models.IntegerField(default=0)
    width = models.IntegerField(default=0)
    height = models.IntegerField(default=0)
    num_rows = models.IntegerField(default=0)
    num_columns = models.IntegerField(default=0)
    spacing_x = models.IntegerField(default=0)
    spacing_y = models.IntegerField(default=0)
