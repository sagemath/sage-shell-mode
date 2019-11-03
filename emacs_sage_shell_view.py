# -*- coding: utf-8 -*-
r"""
Emacs sage-shell-mode Backend for the Sage Rich Output System

This module defines the Emacs backend for :mod:`sage.repl.rich_output`
based on the IPython shell version.

"""

# Copyright (C) 2016 - 2018  Sho Takemori <stakemorii@gmail.com>

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

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
            text = "BEGIN_TEXT:" + str(plain_text.text.get(), 'utf-8') + ":END_TEXTBEGIN_LATEX:" + \
                   str(rich_output.latex.get(), 'utf-8') + ":END_LATEX"
            return ({'text/plain': text}, {})
        else:
            return super(BackendEmacs, self).displayhook(plain_text, rich_output)


def set_backend(text=True, plot=True):
    if text or plot:
        backend = BackendEmacs(text=text, plot=plot)
    else:
        backend = BackendIPythonCommandline()
    backend.get_display_manager().switch_backend(backend, shell=ip)
