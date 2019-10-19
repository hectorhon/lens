from django import forms


class TemplateSelectionsForm(forms.Form):
    selections = forms.CharField(widget=forms.HiddenInput())
