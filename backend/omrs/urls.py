from django.urls import path

from . import views

app_name = 'omrs'

urlpatterns = [

    path('',
         views.index,
         name='index'),

    path('templates',
         views.TemplateListView.as_view(),
         name='template_list'),

    path('templates/create',
         views.TemplateCreateView.as_view(),
         name='template_create_form'),

    path('templates/view/<uuid:pk>',
         views.TemplateDetailView.as_view(),
         name='template_view'),

    path('templates/api/update_template_selections/',
         views.updateTemplateSelections,
         name='template_api_update_template_selections'),
]
