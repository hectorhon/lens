from django.urls import path

from . import views

urlpatterns = [

    path('', views.index, name='index'),

    path('images',
         views.image_list,
         name='image_list'),

    path('images/upload',
         views.upload_image_form,
         name='upload_image_form')
]
