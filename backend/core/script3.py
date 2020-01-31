from django.core.checks import Error, register, Tags


@register(Tags.models)
def example_check(app_configs, **kwargs):
    return [Error('asdf')]
