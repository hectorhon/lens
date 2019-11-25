from django.conf import settings
from django.shortcuts import redirect
from django.urls import reverse


def login_required_middleware(get_response):

    def middleware(request):

        if not request.user.is_authenticated \
           and not request.path == settings.LOGIN_URL \
           and not request.path == reverse('logout'):
            return redirect('%s?next=%s' % (settings.LOGIN_URL, request.path))

        response = get_response(request)

        return response

    return middleware
