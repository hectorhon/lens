from django.core.paginator import Paginator
from django.shortcuts import render, redirect
from django.forms import ModelForm

from .models import Image


def index(request):
    return render(request, 'core/index.html')


def image_list(request):
    page_number = int(request.GET.get('page', 1))
    # limit = max(1, int(request.GET.get('limit', 10)))
    limit = 10
    images = Image.objects.all()
    paginator = Paginator(images, limit)
    image_list = paginator.get_page(page_number)
    return render(request, 'core/image_list.html', {
        'images': image_list,
    })


class ImageForm(ModelForm):
    class Meta:
        model = Image
        fields = ['collection', 'original']


def upload_image_form(request):
    if request.method == 'POST':
        form = ImageForm(request.POST, request.FILES)
        if form.is_valid():
            form.save()
            return redirect('image_list')
    else:
        form = ImageForm()
    return render(request, 'core/upload_image_form.html', {
        'form': form
    })
