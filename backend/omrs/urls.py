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
         views.TemplateSelectionsView.as_view(),
         name='template_view'),

    path('operations',
         views.OperationListView.as_view(),
         name='operation_list'),

    path('operations/create',
         views.OperationCreateView.as_view(),
         name='operation_create_form'),

    path('operations/view/<uuid:pk>',
         views.OperationDetailView.as_view(),
         name='operation_view'),
]
