import mimetypes
import os

import PIL

from django.http import JsonResponse, HttpResponse
from django.core.paginator import Paginator
from django.shortcuts import render, redirect
from django.core.files.images import ImageFile
from django.conf import settings
from django.views import static

from .models import Image, Album
from .forms import ImageUploadForm, AlbumCreateForm


def index(request):
    return render(request, 'core/index.html')


def image_list(request):
    page_number = int(request.GET.get('page', 1))
    limit = 10
    images = Image.objects.all()
    paginator = Paginator(images, limit)
    image_list = paginator.get_page(page_number)
    return render(request, 'core/image_list.html', {
        'images': image_list,
    })


def image_upload_form(request):
    redirect_url = request.GET.get('redirect', 'core:image_list')
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
    return render(request, 'core/image_upload_form.html', {
        'form': form
    })


def image_api_delete(request):
    image_ids = request.POST.getlist('selectedImageIds[]')
    images = Image.objects.filter(pk__in=image_ids)

    # Files to delete
    originals = [image.original.path for image in images]
    thumbnails = [image.thumbnail.path for image in images if image.thumbnail]

    deleteCount, _ = images.delete()

    # Delete the files
    for original in originals:
        os.remove(original)
    for thumbnail in thumbnails:
        os.remove(thumbnail)

    return JsonResponse({
        'deleteCount': deleteCount
    })


def image_api_get(request, image_id):
    image = Image.objects.get(pk=image_id)
    filename = image.original.name
    if os.environ.get('DJANGO_SETTINGS_MODULE') == 'backend.settings_wsgi':
        response = HttpResponse()
        response['X-SendFile'] = f'../uploads/{filename}'
        response['Content-Type'] = mimetypes.guess_type(filename)[0]
        return response
    else:
        return static.serve(request, filename, settings.MEDIA_ROOT)


def image_api_thumbnail(request, image_id):
    image = Image.objects.get(pk=image_id)

    thumbnail_dir = os.path.join(settings.MEDIA_ROOT, 'thumbnails')
    # Ensure directories exist - this should probably be in the setup
    # file because need to set permissions for www-data
    # os.makedirs(thumbnail_dir, exist_ok=True)

    if not image.thumbnail:
        pil_image = PIL.Image.open(image.original.path)

        # .thumbnail edits images in place, so make a copy
        # https://pillow.readthedocs.io/en/stable/reference/Image.html#PIL.Image.Image.thumbnail
        pil_image_copy = pil_image.copy()
        pil_image_copy.thumbnail((400, 400))
        thumbnail_filename = f'{image.original.name}.thumbnail.jpg'

        thumbnail_path = os.path.join(thumbnail_dir, thumbnail_filename)
        try:
            pil_image_copy.save(thumbnail_path, 'jpeg')
        except PermissionError as e:
            # Manual intervention required; log the exception and continue
            # Can happen if the database thumbnail field is cleared but the file isn't deleted
            print(e)
            pass

        # Update database row
        image.thumbnail = os.path.relpath(thumbnail_path, settings.MEDIA_ROOT)
        image.save()

    filename = image.thumbnail.name
    if os.environ.get('DJANGO_SETTINGS_MODULE') == 'backend.settings_wsgi':
        response = HttpResponse()
        response['X-SendFile'] = f'../uploads/{filename}'
        response['Content-Type'] = mimetypes.guess_type(filename)[0]
        return response
    else:
        return static.serve(request, filename, settings.MEDIA_ROOT)


def album_list(request):
    page_number = int(request.GET.get('page', 1))
    limit = 10
    albums = Album.objects.all()
    paginator = Paginator(albums, limit)
    album_list = paginator.get_page(page_number)
    return render(request, 'core/album_list.html', {
        'albums': album_list,
    })


def album_create_form(request):
    if request.method == 'POST':
        form = AlbumCreateForm(request.POST)
        if form.is_valid():
            form.save()
            return redirect('core:album_list')
    else:
        form = AlbumCreateForm()
    return render(request, 'core/album_create_form.html', {
        'form': form
    })


def album_view(request, album_id):
    album = Album.objects.get(pk=album_id)

    page_number = int(request.GET.get('page', 1))
    images = album.image_set.all()
    limit = 8
    paginator = Paginator(images, limit)
    image_list = paginator.get_page(page_number)

    return render(request, 'core/album_view.html', {
        'title': album.name,
        'subtitle': 'Album',
        'album': album,
        'images': image_list,
    })
