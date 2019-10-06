from django.db import models


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
