# -*- coding: utf-8 -*-
import re
import os
from contextlib import contextmanager

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

def print_all_commands(interface):
    if interface == 'sage':
        l = ip.ev('dir()')
        l = [a for a in l if _sage_const_regexp.match(a) is None]
        for a in l:
            print a
    else:
        intfc = ip.ev(interface)
        try:
            for a in intfc.trait_names(verbose=False):
                print a
        except:
            for a in intfc.trait_names():
                print a


def print_all_attributes(varname):
    try:
        var = ip.ev('eval(preparse("%s"))' % (varname))
        if varname in interfaces:
            for a in ip.ev('dir(%s)' % (varname)):
                print a
        elif hasattr(var, 'trait_names'):
            try:
                for a in var.trait_names(verbose=False) + dir(var):
                    print a
            except TypeError:
                for a in var.trait_names() + dir(var):
                    print a
            except:
                for a in dir(var):
                    print a
        else:
            for a in dir(var):
                print a
    except:
        pass



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
        print "In [{k}]: {i}".format(k=k, i=inputs[k])
        print "Out[{k}]: {out}".format(k=k, out=v)
        print delim

_func_call_reg = re.compile("[()]")

def _is_safe_str(s):
    if _func_call_reg.search(s) is None:
        return True
    else:
        return False

def sage_getdef(name):
    if _is_safe_str(name):
        gd_name = "sage.misc.sageinspect.sage_getdef"
        try:
            df = ip.ev("%s(%s)"%(gd_name, name))
            if (df == '( [noargspec] )' and
                ip.ev("hasattr(%s, '__init__')"%name)):
                df = ip.ev("%s(%s.__init__)"%(gd_name, name))
            return "%s%s"%(name, df)
        except NameError:
            pass

_doc_delims = ["EXAMPLE", "EXAMPLES", "TESTS", "AUTHOR", "AUTHORS",
               "ALGORITHM"]

_doc_delim_regexp = re.compile("|".join([_s + ":" for _s in _doc_delims]))

def short_doc(name):
    if _is_safe_str(name):
        sd_name = "sage.misc.sageinspect.sage_getdoc"
        dc = ip.ev("%s(%s)"%(sd_name, name))
        m = _doc_delim_regexp.search(dc)
        if m is not None:
            res = dc[:m.start()]
        else:
            res = dc
        return res.strip()


def print_short_doc(name):
    try:
        print short_doc(name)
    except:
        pass

def print_def(name):
    try:
        df = sage_getdef(name)
        if df is not None:
            print df
    except:
        pass

def print_short_doc_and_def(name):
    try:
        df = sage_getdef(name)
        if df is not None:
            print df
    except:
        df = None

    try:
        if df is not None:
            print ""
        sd = short_doc(name)
        print sd
    except:
        sd = None
