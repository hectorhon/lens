from django.forms import ModelForm, Form, \
    FileField, ModelChoiceField, UUIDField, \
    ClearableFileInput, HiddenInput

from .models import Album


class ImageUploadForm(Form):
    files = FileField(widget=ClearableFileInput(attrs={
        'multiple': True
    }))
    album = ModelChoiceField(
        queryset=Album.objects.all(),
        required=False,
    )
    # because select doesn't have html readonly attribute
    # https://stackoverflow.com/questions/368813/html-form-readonly-select-tag-input
    hidden_album_id = UUIDField(
        widget=HiddenInput(),
        required=False,
    )

    
class AlbumCreateForm(ModelForm):
    class Meta:
        model = Album
        fields = ['name']
