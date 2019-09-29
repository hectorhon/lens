from django.http import HttpResponse
from django.shortcuts import render, redirect
from django.forms import ModelForm

from .models import Template, Image


def index(request):
    templates = Template.objects.all()[:10]
    output = ', '.join([str(q.id) for q in templates])
    return HttpResponse(output)


def image_list(request):
    start = request.GET.get('skip', 0)
    end = request.GET.get('limit', 10)
    images = Image.objects.all()[start:end]
    context = {
        'images': images
    }
    return render(request, 'core/image_list.html', context)


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
