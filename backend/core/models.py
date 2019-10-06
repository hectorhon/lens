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
