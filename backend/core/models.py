from django.db import models

# Create your models here.


class Collection(models.Model):
    pass


class Image(models.Model):
    collection = models.ForeignKey(Collection, null=True, blank=True, on_delete=models.SET_NULL)
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
