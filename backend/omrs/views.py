from django.shortcuts import render
from django.views import generic
from django.urls import reverse_lazy

from .models import Template


def index(request):
    return render(request, 'omrs/index.html')


class TemplateListView(generic.ListView):
    model = Template
    paginate_by = 2

    def get_context_data(self, **kwargs):
        context = super().get_context_data(**kwargs)
        context['title'] = 'Templates'
        return context


class TemplateCreateView(generic.CreateView):
    model = Template
    fields = ['name', 'base_image']
    success_url = reverse_lazy('omrs:template_list')


class TemplateDetailView(generic.DetailView):
    model = Template
