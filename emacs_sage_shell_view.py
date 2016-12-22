# -*- coding: utf-8 -*-
r"""
Emacs sage-shell-mode Backend for the Sage Rich Output System

This module defines the Emacs backend for :mod:`sage.repl.rich_output`
based on the IPython shell version.

"""

from emacs_sage_shell import ip
from sage.repl.rich_output.output_basic import OutputLatex

from sage.repl.rich_output.output_catalog import OutputImagePng
from sage.repl.rich_output.preferences import DisplayPreferences
from sage.repl.rich_output.backend_ipython import BackendIPythonCommandline


class BackendEmacs(BackendIPythonCommandline):

    def __init__(self, text=True, plot=True):
        super(BackendEmacs, self).__init__()
        if text:
            self.__text = "latex"
        else:
            self.__text = None
        self.__plot = plot

    def default_preferences(self):
        return DisplayPreferences(text=self.__text)

    def _repr_(self):
        return "Emacs babel"

    def displayhook(self, plain_text, rich_output):
        if self.__plot and isinstance(rich_output, OutputImagePng):
            msg = rich_output.png.filename(ext='png')
            msg = "BEGIN_PNG:%s:END_PNG" % msg
            return ({u'text/plain': msg}, {})

        elif isinstance(rich_output, OutputLatex):
            text = "BEGIN_TEXT:" + plain_text.text.get() + ":END_TEXT\nBEGIN_LATEX:" + \
                   rich_output.latex.get() + ":END_LATEX"
            return ({u'text/plain': text}, {})
        else:
            return super(BackendEmacs, self).displayhook(plain_text, rich_output)


def set_backend(text=True, plot=True):
    if text or plot:
        backend = BackendEmacs(text=text, plot=plot)
    else:
        backend = BackendIPythonCommandline()
    backend.get_display_manager().switch_backend(backend, shell=ip)
