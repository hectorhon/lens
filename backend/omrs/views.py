from django.shortcuts import render
from django.core.paginator import Paginator

from .models import Template


def index(request):
    return render(request, 'omrs/index.djhtml')


def template_list(request):
    page_number = int(request.GET.get('page', 1))
    limit = 10
    images = Template.objects.all()
    paginator = Paginator(images, limit)
    template_list = paginator.get_page(page_number)
    return render(request, 'omrs/template_list.djhtml', {
        'title': 'Templates',
        'templates': template_list,
    })
