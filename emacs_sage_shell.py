# -*- coding: utf-8 -*-
import re
try:
    ip = get_ipython()
    ip.autoindent = False
except:                         # Older versions
    import IPython.ipapi
    ip = IPython.ipapi.get()
    ip.IP.shell.autoindent = False

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
