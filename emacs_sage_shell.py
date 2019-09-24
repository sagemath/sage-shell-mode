# -*- coding: utf-8 -*-
# Copyright (C) 2016 - 2018 Sho Takemori <stakemorii@gmail.com>

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

from __future__ import print_function

import inspect
import os
import re
import sys
from contextlib import contextmanager

import IPython
from IPython.core.completerlib import module_completion

import sage
from sage.all import preparse

try:
    ip = get_ipython()
    ip.autoindent = False
except:                         # Older versions
    import IPython.ipapi
    ip = IPython.ipapi.get()
    ip.IP.shell.autoindent = False

# Disable highlighting matching parentheses.
try:
    if IPython.version_info[0] >= 5:
        sage.repl.interpreter.SageTerminalInteractiveShell.highlight_matching_brackets = False
except:
    pass

# Disable color.
ip.run_line_magic('colors', 'NoColor')

# Disable the SQLite history.
try:
    ip.run_line_magic("config", "HistoryManager.enabled = False")
except:
    pass

interfaces = ip.ev('interfaces')

_sage_const_regexp = re.compile("_sage_const_")


class Memorize(object):

    def __init__(self, f):
        self._f = f
        self._cached = {}

    def __call__(self, *args):
        if args in self._cached:
            return self._cached[args]
        else:
            res = self._f(*args)
            self._cached[args] = res
            return res


memorize = Memorize


def print_cpl_sexp(typs, compl_dct):
    def _to_lisp_str_ls(ls):
        return "(%s)" % " ".join(['"%s"' % (a, ) for a in ls])

    funcs = {"interface": all_commands,
             "attributes": all_attributes,
             "modules": all_modules,
             "vars-in-module": all_vars_in_module,
             "in-function-call": all_keyword_args}
    alst = [(tp, funcs[tp](compl_dct)) for tp in typs]
    conss = ['("%s" . %s)' % (tp, _to_lisp_str_ls(ls))
             for tp, ls in alst if ls is not None]
    print("(" + "".join(conss) + ")")


def all_modules(compl_dct):
    try:
        module_name = compl_dct["module-name"]
        return _all_modules(module_name)
    except:
        return []


def _all_modules(module_name):
    if module_name is None:
        return list_modules_in_syspath()
    else:
        return [a.split(".")[-1] for a in
                module_completion("import %s." % (module_name, ))]


def all_vars_in_module(compl_dct):
    try:
        module_name = compl_dct["module-name"]
        return _all_vars_in_module(module_name)
    except:
        return []


special_att_regexp = re.compile("__[a-zA-Z0-9_]+__")


def _all_vars_in_module(module_name):
    if module_name is None:
        return []

    # If imported module, use dir.
    if module_name in sys.modules:
        res = dir(sys.modules[module_name])
    else:
        p = resolve_module_path(module_name)
        if p is None:
            # If resolving fails, use module_completion.
            return module_completion("from %s import " % (module_name, ))
        res = None
        if os.path.isdir(p):
            res = list_modules_in(p)

        # Ohterwise, parse the file.
        if res is None:
            res = []
            regexp = re.compile(
                "^{name} *= *|^def +{name}|^class +{name}".format(
                    name="([a-zA-Z0-9_]+)"))
            with open(p) as f:
                for l in f:
                    m = regexp.match(l)
                    if m is not None:
                        res.extend([c for c in m.groups() if c is not None])
    res = [a for a in res if special_att_regexp.match(a) is None]
    return res


def all_commands(compl_dct):
    interface = compl_dct["interface"]
    if interface == 'sage':
        l = ip.ev('dir()')
        l = [a for a in l if _sage_const_regexp.match(a) is None]
        return l
    else:
        intfc = ip.ev(interface)
        if isinstance(intfc, sage.interfaces.expect.Expect):
            return list(sorted(_completions_attributes(interface)))
        else:
            return []


def all_attributes(compl_dct):
    varname = compl_dct["var-base-name"]
    try:
        regexp = re.compile("^[ a-zA-Z0-9._\\[\\]]+$")
        if regexp.match(varname) is None:
            return []
        if varname in interfaces:
            ls = ip.ev('dir(%s)' % (varname,))
        else:
            ls = _completions_attributes(preparse(varname))
            ls.extend(ip.ev('dir(%s)' % (preparse(varname),)))
            ls = list(sorted(set(ls)))
        return ls
    except:
        return []


def _completions_attributes(varname):
    completions = ip.complete('%s.' % (varname, ))[1]
    ln = len(varname) + 1
    return [a[ln:] for a in completions]


def list_modules_in(p):
    res = [os.path.basename(a) for a in list_module_paths_in(p)]
    return [os.path.splitext(a)[0] for a in res]


def list_module_paths_in(p):
    if not os.path.exists(p):
        return []
    elif os.path.isdir(p):
        res = []
        for f in os.listdir(p):
            a = is_module(os.path.join(p, f))
            if a:
                res.append(a)
        return res
    else:
        return []


mod_regexp = re.compile("^[A-Za-z0-9_.]+$")


def is_module(p):
    if not re.match(mod_regexp, os.path.basename(p)):
        return False
    elif os.path.isfile(p):
        if p.endswith("py"):
            return p
    elif os.path.isdir(p):
        if os.path.exists(os.path.join(p, "__init__.py")):
            return p


@memorize
def list_module_paths_in_syspath():
    res = []
    for p in sys.path:
        res.extend(list_module_paths_in(p))
    return res


def list_modules_in_syspath():
    return module_completion("import ")


@memorize
def resolve_module_path(modname):
    lmis = list_module_paths_in_syspath()
    root_mod_name = modname.split(".")[0]
    ls = [a for a in lmis
          if os.path.splitext(os.path.basename(a))[0] == root_mod_name]
    if ls == []:
        return None
    root_path = ls[0]
    pth = os.path.join(os.path.dirname(root_path),
                       os.path.sep.join(modname.split(".")))
    if os.path.isdir(pth):
        return pth
    for ext in [".py", ".pyx"]:
        _pth = pth + ext
        if os.path.isfile(_pth):
            return _pth


def lazy_import_get_obj(obj):
    _max = 10
    i = 0
    while (isinstance(obj, sage.misc.lazy_import.LazyImport) and
           i < _max):
        obj = obj._get_object()
        i += 1
    return obj


def source_line(obj):
    return sage.misc.sageinspect.sage_getsourcelines(obj)[-1]


def print_source_file_and_line_num(obj):
    obj = lazy_import_get_obj(obj)
    sf = sage.misc.sageinspect.sage_getfile(obj)
    sl = source_line(obj)
    print(sf, '*', sl)


def print_source_line(obj):
    print(source_line(obj))


def print_sage_root():
    print(os.environ['SAGE_ROOT'])


@contextmanager
def current_dir(d):
    cwd = os.getcwd()
    os.chdir(d)
    try:
        yield
    finally:
        os.chdir(cwd)


def sage_tex_load(f):
    d = os.path.dirname(os.path.expanduser(f))
    with current_dir(d):
        ip.ev('load("{f}")'.format(f=f))


def print_inputs_outputs(max_line_num, delim, reversed_ord):
    def show_func(s):
        if max_line_num is None:
            res = s
        else:
            ss = s.split("\n")
            if len(ss) > max_line_num:
                l = ss[:max_line_num] + ["....."]
            else:
                l = ss
            res = "\n".join(l)
        if '\n' in res:
            return '\n' + res
        else:
            return res

    def format_func(obj):
        try:
            if hasattr(obj, "show"):
                return repr(obj)
            else:
                return ip.display_formatter.format(obj)[0]['text/plain']
        except:
            return repr(obj)

    outputs = ip.ev("_oh")
    if reversed_ord:
        def key_func(x):
            return -x[0]
    else:
        def key_func(x):
            return x
    outputs = sorted(list(outputs.items()), key=key_func)
    outputs = [(k, show_func(format_func(v))) for k, v in outputs]
    inputs = ip.ev("_ih")
    # TODO: Find a better way.
    regexp = re.compile(
        r'_emacs_sage_shell\.run_cell_and_print_msg_id\("_emacs_ob_sagemath\.run_cell_babel.+')
    for k, v in outputs:
        if regexp.match(inputs[k]) is None:
            print("In [{k}]: {i}".format(k=k, i=inputs[k]))
            print("Out[{k}]: {out}".format(k=k, out=v))
            print(delim)


def _is_safe_str(s):
    _func_call_reg = re.compile("[()]")
    return _func_call_reg.search(s) is None


def print_info(name):
    run_cell("%s?" % (name,))


ignore_classes = [sage.interfaces.gap.Gap, sage.misc.lazy_import.LazyImport]


def _sage_getdef(name, base_name=None):
    try:
        if _is_safe_str(name) and (_should_be_ignored(name, base_name, clses=[])
                                   is not None):
            gd_name = "sage.misc.sageinspect.sage_getdef"
            name_ob = ip.ev(preparse(name))
            name_ob = lazy_import_get_obj(name_ob)
            if inspect.isclass(name_ob):
                df = ip.ev("%s(%s.__init__)" % (gd_name, name))
            else:
                df = ip.ev("%s(%s)" % (gd_name, preparse(name)))
            return df
    except NameError:
        pass


def sage_getdef(name, base_name=None):
    df = _sage_getdef(name, base_name=base_name)
    if df is not None:
        return "%s%s" % (name, df)


_doc_delims = ["EXAMPLE", "EXAMPLES", "TESTS", "AUTHOR", "AUTHORS",
               "ALGORITHM"]

_doc_delim_regexp = re.compile("|".join([_s + ":" for _s in _doc_delims]))


def _should_be_ignored(name, base_name, clses=None):
    if clses is None:
        clses = ignore_classes
    if isinstance(base_name, str):
        base_ob = ip.ev(preparse(base_name))
        if any((isinstance(base_ob, cls) for cls in clses)):
            return None
    else:
        base_ob = None
    name_ob = ip.ev(preparse(name))
    if any((isinstance(name_ob, cls) for cls in clses)):
        return None
    else:
        return True


def short_doc(name, base_name=None):
    '''
    If name or base_name is an instance of one of ignore_classes,
    then this function returns None.
    '''
    sd_name = "sage.misc.sageinspect.sage_getdoc"
    if _is_safe_str(name) and (_should_be_ignored(name, base_name)
                               is not None):
        dc = ip.ev("%s(%s)" % (sd_name, preparse(name)))
        m = _doc_delim_regexp.search(dc)
        if m is not None:
            res = dc[:m.start()]
        else:
            res = dc
        return res.strip()


def all_keyword_args(compl_dct):
    try:
        base_name = compl_dct["in-function-call-base-name"]
        name = compl_dct["in-function-call"]
        return keyword_args(name, base_name=base_name)
    except:
        return []


def keyword_args(name, base_name=None):
    gd = _sage_getdef(name, base_name=base_name)
    no_argspec_reg = re.compile(r"\[noargspec\]")
    if (not gd) or re.match(no_argspec_reg, gd):
        return []
    else:
        args_str = gd[1:-1]
        reg = re.compile(r"\*+[a-zA-Z_0-9]+")
        args = args_str.split(", ")
        args = [a for a in args if reg.match(a) is None]
        reg = re.compile("[a-zA-Z_0-9]+")
        matches = [reg.match(a) for a in args]
        return [m.group() + "=" for m in matches if m]


def print_short_doc(name, base_name=None):
    try:
        sd = short_doc(name, base_name=base_name)
        if sd is not None:
            print(sd)
    except:
        pass


def print_def(name, base_name=None):
    try:
        df = sage_getdef(name, base_name=base_name)
        if df is not None:
            print(df)
    except:
        pass


def print_short_doc_and_def(name, base_name=None):
    try:
        df = sage_getdef(name, base_name=base_name)
        if df is not None:
            print(df)
    except:
        df = None

    try:
        sd = short_doc(name, base_name=base_name)
        if df is not None and sd is not None:
            print("")
        if sd is not None:
            print(sd)
    except:
        pass


def run_cell(code):
    res = ip.run_cell(code)
    ip.set_next_input("")
    return res


def run_cell_and_print_state(code, msg_id_start, msg_id_end):
    print(msg_id_start)
    res = run_cell(code)
    if hasattr(res, 'success'):
        if res.success:
            print(0)
        else:
            print(1)
    else:
        # For old Sages. It always succeeds.
        print(0)
    print(msg_id_end)


def run_cell_and_print_msg_id(code, msg_id_start, msg_id_end):
    print(msg_id_start)
    run_cell(code)
    print(msg_id_end)


def read_file_and_run_cell(filename):
    with open(filename, "r") as fp:
        contents = fp.read()
    return ip.run_cell(contents)


def read_file_and_run_contents(filename):
    read_file_and_run_cell(filename)
