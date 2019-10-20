from django.shortcuts import render
from django.views import generic
from django.urls import reverse_lazy

import json
import logging

from .models import Template, Selection, Operation
from .forms import TemplateSelectionsForm

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


class TemplateSelectionsView(generic.FormView):
    """View and update the selections for this template."""

    template_name = 'omrs/template_selections.html'
    form_class = TemplateSelectionsForm

    def get_initial(self):
        # get_initial is also being called during POST. Huh.
        if (self.request.method == 'POST'):
            return {}

        template_id = self.kwargs['pk']
        selections = Selection.objects.filter(template_id=template_id)
        return {
            'selections': Template.selections_to_json(selections)
        }

    def get_success_url(self):
        return reverse_lazy('omrs:template_view',
                            kwargs={'pk': self.kwargs['pk']})

    def form_valid(self, form):
        template_id = self.kwargs['pk']
        json_string = form.cleaned_data['selections']
        logger.debug(json.dumps(json.loads(json_string), indent=4))
        selections = Template.parse_selections(json_string, template_id)
        Selection.objects.filter(template_id=template_id).delete()
        Selection.objects.bulk_create(selections)
        return super().form_valid(form)

    def get_context_data(self, **kwargs):
        context = super().get_context_data(**kwargs)
        template_id = self.kwargs['pk']
        template = Template.objects.get(pk=template_id)
        context['template'] = template
        context['title'] = template.name
        context['subtitle'] = 'Template'
        return context


class OperationListView(generic.ListView):
    queryset = Operation.objects.order_by('-created_on').all()
    paginate_by = 10

    def get_context_data(self, **kwargs):
        context = super().get_context_data(**kwargs)
        context['title'] = 'Operations'
        return context


class OperationCreateView(generic.CreateView):
    model = Operation
    fields = ['template', 'album']
    success_url = reverse_lazy('omrs:operation_list')


class OperationDetailView(generic.DetailView):
    model = Operation

    def get_context_data(self, **kwargs):
        context = super().get_context_data(**kwargs)
        context['title'] = ''
        context['subtitle'] = 'Operation'
        return context
