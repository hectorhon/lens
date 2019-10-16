from django.shortcuts import render
from django.views import generic
from django.urls import reverse_lazy
from django.http import HttpResponse

import json
import logging

from .models import Template, Selection

logger = logging.getLogger(__name__)


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
    fields = ['name', 'base_image']

    def get_context_data(self, **kwargs):
        context = super().get_context_data(**kwargs)
        context['title'] = self.object.name
        context['subtitle'] = 'Template'
        return context


def updateTemplateSelections(request):
    parsed_json = json.loads(request.body)
    logger.debug(json.dumps(parsed_json, indent=4))
    template_id = parsed_json['templateId']
    selections = parsed_json['selectionsInCreationOrder']
    Selection.objects.filter(template_id=template_id).delete()
    Selection.objects.bulk_create([Selection(
        id=selection['id'],
        template_id=template_id,
        order=index,
        name=selection['name'],
        x=selection['x'],
        y=selection['y'],
        width=selection['width'],
        height=selection['height'],
        num_rows=selection['numRows'],
        num_columns=selection['numColumns'],
        spacing_x=selection['spacingX'],
        spacing_y=selection['spacingY'],
    ) for index, selection in enumerate(selections, start=1)])
    return HttpResponse()
