# -*- coding: utf-8 -*-
from __future__ import print_function
import re
import sys
import os
from contextlib import contextmanager
import sage
from sage.all import preparse
import inspect
from IPython.core.completerlib import module_completion

try:
    ip = get_ipython()
    ip.autoindent = False
except:                         # Older versions
    import IPython.ipapi
    ip = IPython.ipapi.get()
    ip.IP.shell.autoindent = False

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
            try:
                return intfc.trait_names(verbose=False)
            except:
                try:
                    return intfc.trait_names()
                except:
                    return ip.ev('dir(%s)' % (interface, ))
        else:
            return []


def all_attributes(compl_dct):
    varname = compl_dct["var-base-name"]
    try:
        regexp = re.compile("^[ a-zA-Z0-9._\\[\\]]+$")
        if regexp.match(varname) is None:
            return []
        var = ip.ev(preparse(varname))
        if varname in interfaces:
            ls = ip.ev('dir(%s)' % (varname))
        elif hasattr(var, 'trait_names'):
            try:
                ls = var.trait_names(verbose=False) + dir(var)
            except TypeError:
                ls = var.trait_names() + dir(var)
            except:
                ls = dir(var)
        else:
            ls = dir(var)

        return ls
    except:
        return []


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


def source_line(obj):
    return sage.misc.sageinspect.sage_getsourcelines(obj)[-1]


def print_source_file_and_line_num(obj):
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
        key_func = lambda x: -x[0]
    else:
        key_func = lambda x: x
    outputs = sorted(list(outputs.items()), key=key_func)
    outputs = [(k, show_func(format_func(v))) for k, v in outputs]
    inputs = ip.ev("_ih")
    # TODO: Find a better way.
    regexp = re.compile(
        r'_emacs_sage_shell\.run_cell_dummy_prompt\("_emacs_ob_sagemath\.run_cell_babel.+')
    for k, v in outputs:
        if regexp.match(inputs[k]) is None:
            print("In [{k}]: {i}".format(k=k, i=inputs[k]))
            print("Out[{k}]: {out}".format(k=k, out=v))
            print(delim)

_func_call_reg = re.compile("[()]")


def _is_safe_str(s):
    if _func_call_reg.search(s) is None:
        return True
    else:
        return False


def print_info(name):
    ip.run_cell("%s?" % (name,))
    # In some cases, the next line is not blank.
    ip.set_next_input("")

ignore_classes = [sage.interfaces.gap.Gap, sage.misc.lazy_import.LazyImport]


def _sage_getdef(name, base_name=None):
    try:
        if _is_safe_str(name) and (_should_be_ignored(name, base_name)
                                   is not None):
            gd_name = "sage.misc.sageinspect.sage_getdef"
            name_ob = ip.ev(preparse(name))
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


def _should_be_ignored(name, base_name):
    if isinstance(base_name, str):
        base_ob = ip.ev(preparse(base_name))
        if any((isinstance(base_ob, cls) for cls in ignore_classes)):
            return None
    else:
        base_ob = None
    name_ob = ip.ev(preparse(name))
    if any((isinstance(name_ob, cls) for cls in ignore_classes)):
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


def run_cell_dummy_prompt(code, dummy):
    ip.run_cell(code)
    print(dummy)
