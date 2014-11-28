# -*- coding: utf-8 -*-
import re
import os
from contextlib import contextmanager

try:
    from IPython import get_ipython
    ip = get_ipython()
    ip.autoindent = False
except:                         # Older versions
    import IPython.ipapi
    ip = IPython.ipapi.get()
    ip.IP.shell.autoindent = False

# Disable color.
ip.run_line_magic('colors', 'NoColor')

interfaces = ip.ev('interfaces')


def print_all_commands(interface):
    if interface == 'sage':
        p = re.compile("_sage_const_")
        l = ip.ev('dir()')
        l = [a for a in l if p.match(a) is None]
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
