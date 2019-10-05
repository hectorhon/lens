from django.db import models
from uuid import uuid4

# Create your models here.


class Album(models.Model):
    id = models.UUIDField(primary_key=True, default=uuid4, editable=False)
    name = models.CharField(max_length=200)


class Image(models.Model):
    id = models.UUIDField(primary_key=True, default=uuid4, editable=False)
    album = models.ForeignKey(Album, null=True, blank=True, on_delete=models.SET_NULL)
    original = models.ImageField()
    thumbnail = models.ImageField()


class Template(models.Model):
    pass


class Selection(models.Model):
    id = models.UUIDField(primary_key=True, editable=False)
    template = models.ForeignKey(Template, on_delete=models.CASCADE)
    name = models.CharField(max_length=200)
    x = models.IntegerField(default=0)
    y = models.IntegerField(default=0)
    width = models.IntegerField(default=0)
    height = models.IntegerField(default=0)
    numRows = models.IntegerField(default=0)
    numColumns = models.IntegerField(default=0)
    spacingX = models.IntegerField(default=0)
    spacingY = models.IntegerField(default=0)
