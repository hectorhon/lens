from django.core.paginator import Paginator
from django.shortcuts import render, redirect
from django.core.files.images import ImageFile

from .models import Image, Album
from .forms import ImageUploadForm, AlbumCreateForm


def index(request):
    return render(request, 'core/index.djhtml')


def image_list(request):
    page_number = int(request.GET.get('page', 1))
    limit = 10
    images = Image.objects.all()
    paginator = Paginator(images, limit)
    image_list = paginator.get_page(page_number)
    return render(request, 'core/image_list.djhtml', {
        'images': image_list,
    })


def image_upload_form(request):
    redirect_url = request.GET.get('redirect', 'image_list')
    preset_album = request.GET.get('album')
    if request.method == 'POST':
        form = ImageUploadForm(request.POST, request.FILES)
        if form.is_valid():
            album = form.cleaned_data['album']
            if album is None:
                # maybe select field is disabled, try the hidden input
                album_id_from_hidden_input = form.cleaned_data['hidden_album_id']
                if album_id_from_hidden_input is not None:
                    album = Album.objects.get(pk=album_id_from_hidden_input)
            # https://docs.djangoproject.com/en/2.2/topics/http/file-uploads/#uploading-multiple-files
            # https://stackoverflow.com/questions/46318587/django-uploading-multiple-files-list-of-files-needed-in-cleaned-datafile
            for file in request.FILES.getlist('files'):
                image = Image(
                    album=album,
                    original=ImageFile(file),
                )
                image.save()
            return redirect(redirect_url)
    else:
        if preset_album is not None:
            form = ImageUploadForm(initial={
                'album': preset_album,
                'hidden_album_id': preset_album,
            })
            form.fields['album'].disabled = True
        else:
            form = ImageUploadForm()
    return render(request, 'core/image_upload_form.djhtml', {
        'form': form
    })


def album_list(request):
    page_number = int(request.GET.get('page', 1))
    limit = 10
    albums = Album.objects.all()
    paginator = Paginator(albums, limit)
    album_list = paginator.get_page(page_number)
    return render(request, 'core/album_list.djhtml', {
        'albums': album_list,
    })


def album_create_form(request):
    if request.method == 'POST':
        form = AlbumCreateForm(request.POST)
        if form.is_valid():
            form.save()
            return redirect('album_list')
    else:
        form = AlbumCreateForm()
    return render(request, 'core/album_create_form.djhtml', {
        'form': form
    })


def album_view(request):
    album_id = request.GET.get('id')
    album = Album.objects.get(pk=album_id)
    return render(request, 'core/album_view.djhtml', {
        'album': album
    })
