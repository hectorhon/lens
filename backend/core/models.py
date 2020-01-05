from django.db import models
from uuid import uuid4


class Album(models.Model):
    id = models.UUIDField(primary_key=True, default=uuid4, editable=False)
    name = models.CharField(max_length=200)

    def __str__(self):
        return '%s' % (self.name)


class Image(models.Model):
    id = models.UUIDField(primary_key=True, default=uuid4, editable=False)
    album = models.ForeignKey(Album, null=True, blank=True, on_delete=models.SET_NULL)
    uploaded_on = models.DateTimeField(auto_now_add=True)
    original = models.ImageField()
    thumbnail = models.ImageField()

    # A timestamp associated with the image, if available
    image_date = models.DateTimeField(null=True, blank=True)

    # e.g. for local disk indexers, the original directory containing the image
    source = models.CharField(max_length=200)

    # always 256 bits / 32 bytes / 64 hex characters
    sha256_checksum = models.CharField(max_length=64)
