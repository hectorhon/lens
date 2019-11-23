from django.db import connection
from django.conf import settings

import logging

logger = logging.getLogger(__name__)

def debug_sql_middleware(get_response):

    def middleware(request):

        response = get_response(request)

        if not settings.DEBUG or len(connection.queries) == 0:
            return response

        for query in connection.queries:
            logger.debug(query['sql'])

        return response

    return middleware
