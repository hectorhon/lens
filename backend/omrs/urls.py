from django.urls import path

from . import views

app_name = 'omrs'

urlpatterns = [

    path('',
         views.index,
         name='index'),

    path('templates',
         views.template_list,
         name='template_list'),
]
