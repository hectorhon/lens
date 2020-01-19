from django.urls import path

from . import views

app_name = 'core'

urlpatterns = [

    path('',
         views.index,
         name='index'),

    path('images',
         views.image_list,
         name='image_list'),

    path('images/upload',
         views.image_upload_form,
         name='image_upload_form'),

    path('images/api/delete',
         views.image_api_delete,
         name='image_api_delete'),

    path('albums',
         views.album_list,
         name='album_list'),

    path('albums/create',
         views.album_create_form,
         name='album_create_form'),

    path('albums/view/<uuid:album_id>',
         views.album_view,
         name='album_view'),
]
