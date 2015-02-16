# -*- coding: utf-8 -*-
from __future__ import print_function
import re
import os
from contextlib import contextmanager
import sage

try:
    ip = get_ipython()
    ip.autoindent = False
except:                         # Older versions
    import IPython.ipapi
    ip = IPython.ipapi.get()
    ip.IP.shell.autoindent = False

# Disable color.
ip.run_line_magic('colors', 'NoColor')

interfaces = ip.ev('interfaces')

_sage_const_regexp = re.compile("_sage_const_")


def print_all_commands(interface, delim=None):
    if delim:
        print(delim)
    if interface == 'sage':
        l = ip.ev('dir()')
        l = [a for a in l if _sage_const_regexp.match(a) is None]
        for a in l:
            print(a)
    else:
        intfc = ip.ev(interface)
        try:
            for a in intfc.trait_names(verbose=False):
                print(a)
        except:
            for a in intfc.trait_names():
                print(a)


def print_all_attributes(varname):
    try:
        var = ip.ev('eval(preparse("%s"))' % (varname))
        if varname in interfaces:
            ls = ip.ev('dir(%s)' % (varname))
        elif hasattr(var, 'trait_names'):
            try:
                ls = var.trait_names(verbose=False) + dir(var)
            except TypeError:
                ls = var.trait_names() + dir(var)
            except:
                ls = dir(var)
        elif hasattr(var, '__file__'):
            ls = list_submodules(var) + dir(var)
        else:
            ls = dir(var)

        for a in ls:
            print(a)
    except:
        pass



def list_submodules(module):
    fl = module.__file__
    if os.path.splitext(os.path.basename(fl))[0] != "__init__":
        return []
    try:
        drct = os.path.dirname(fl)
        def is_submodule(p):
            if os.path.isfile(p):
                return  os.path.splitext(p)[1] in [".py", ".pyc"]
            elif os.path.isdir(p):
                init_file = os.path.join(p, "__init__.py")
                return os.path.exists(init_file)
            else:
                return False

        l = [os.path.basename(f) for f in os.listdir(drct)
             if is_submodule(os.path.join(drct, f))]
        l = [os.path.splitext(p)[0] for p in l]
        return sorted(list(set(l)))
    except:
        return []


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
    for k, v in outputs:
        print("In [{k}]: {i}".format(k=k, i=inputs[k]))
        print("Out[{k}]: {out}".format(k=k, out=v))
        print(delim)

_func_call_reg = re.compile("[()]")


def _is_safe_str(s):
    if _func_call_reg.search(s) is None:
        return True
    else:
        return False

ignore_classes = [sage.interfaces.gap.Gap, sage.misc.lazy_import.LazyImport]


def sage_getdef(name, base_name=None):
    import inspect
    if _is_safe_str(name) and (_should_be_ignored(name, base_name)
                               is not None):
        gd_name = "sage.misc.sageinspect.sage_getdef"
        try:
            name_ob = ip.ev(name)
            if inspect.isclass(name_ob):
                df = ip.ev("%s(%s.__init__)"%(gd_name, name))
            else:
                df = ip.ev("%s(%s)"%(gd_name, name))
            return "%s%s"%(name, df)
        except NameError:
            pass

_doc_delims = ["EXAMPLE", "EXAMPLES", "TESTS", "AUTHOR", "AUTHORS",
               "ALGORITHM"]

_doc_delim_regexp = re.compile("|".join([_s + ":" for _s in _doc_delims]))


def _should_be_ignored(name, base_name):
    name_ob = ip.ev(name)
    if isinstance(base_name, str):
        base_ob = ip.ev(base_name)
    else:
        base_ob = None
    if any(isinstance(base_ob, cls) or isinstance(name_ob, cls)
           for cls in ignore_classes):
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
        dc = ip.ev("%s(%s)"%(sd_name, name))
        m = _doc_delim_regexp.search(dc)
        if m is not None:
            res = dc[:m.start()]
        else:
            res = dc
        return res.strip()


def print_short_doc(name, base_name=None):
    try:
        print(short_doc(name, base_name=base_name))
    except:
        pass


def print_def(name):
    try:
        df = sage_getdef(name)
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
