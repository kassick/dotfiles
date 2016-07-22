#!/usr/bin/env python
# -*- coding: utf-8 -*-

# File: "/home/kassick/Sources/dotfiles/install.py"
# Created: "Thu Jul 21 21:46:45 2016"
# Updated: "2016-07-22 13:49:32 kassick"
# $Id$
# Copyright (C) 2016, Rodrigo Kassick

import os
import os.path as path
import sys
import re
import subprocess

SELECTION_FILE="dot_select.txt"

def add_dot(dots, name, path, install, bundle):
    if name in dots:
        dots[name] = tuple(map(lambda i: i[0] if i[1] is None else i[1],
                           zip([path, install, bundle], dots[name])))
    else:
        dots[name] = (path, install, bundle)

def rec_file_split(p):
    if len(p) == 0:
        return []
    elif p == '/':
        return ['/']
    else:
        (h, t) = path.split(p)
        return rec_file_split(h) + [t]


def files_conflict_resolve(src, dst, renames, removes, installs):
    if (os.path.islink(dst)):
        # exists and is a link
        removes.append(dst)
        installs.append(( os.path.abspath( src ), dst) )
    elif os.path.exists(dst):
        # Exists and is not a link
        print "\tFile %s already exists" % dst
        print "\tAction:"
        print "\t\t (o)verride"
        print "\t\t (b)ackup"
        print "\t\t (s)kip"

        action = None
        while not(action in ['o', 'b', 's']):
            action = sys.stdin.readline().strip()

        if action == 'o':
            removes.append(dst)
            installs.append( (os.path.abspath( src ), dst) )
        elif action == 'b':
            renames.append( (dst, dst + '.orig') )
            installs.append( (os.path.abspath( src ), dst) )
        pass
    else:
        installs.append( (os.path.abspath(src), dst) )

def dot_install(dot, dot_path, dot_install, dot_bundle):
    removes = []
    renames = []
    dirs = []
    installs = []
    executes = []

    if dot_path:
        print "dot/%s --> ~/.%s" % (dot_path, dot_path)

        dest = path.expanduser(path.join('~', '.' + dot_path ))
        src = path.join('dot', dot_path)

        files_conflict_resolve(src, dest, renames, removes, installs )

    if dot_bundle:
        print "Install bundle %s " % dot
        for root, dirnames, filenames in os.walk(path.join('bundle', dot)):
            destbase = os.path.sep.join(rec_file_split(root)[2:]) # ditch bundle/dotname

            for d in dirnames:
                do_mkdir = True
                if len(destbase) == 0:
                    dest_dir = os.path.expanduser(os.path.join("~", "." + d))
                else:
                    dest_dir = os.path.expanduser(os.path.join('~', '.' + destbase, d))
                if os.path.exists(dest_dir):
                    if os.path.isdir(dest_dir):
                        do_mkdir = False
                    else:
                        print "\tTarget %s is not a directory" % destk_dir
                        print "\t\t (b)ackup"
                        print "\t\t (i)gnore"
                        action = None
                        while not(action in ['b', 'i']):
                            action = sys.stdin.readline().strip()

                        if action == 'b':
                            os.rename(dest_dir, dest_dir + ".orig")
                        else:
                            do_mkdir = False
                        pass
                    pass

                if do_mkdir:
                    dirs.append(dest_dir)

            for f in filenames:
                if len(destbase) == 0:
                    dest_path = os.path.expanduser(os.path.join("~", '.' + f))
                else:
                    dest_path = os.path.expanduser(os.path.join("~", "." + destbase, f))

                src_file = path.join(root, f)

                files_conflict_resolve(src_file, dest_path, renames, removes, installs )
            pass
        pass

    if dot_install:
        executes.append(dot_install)

    return (removes, renames, dirs, installs, executes)

def do_all_actions(removes, renames, dirs, installs, executes):
    print "Verification:"
    print "Will remove:"
    for f in removes:
        print "\t", f
    print ""
    print "Will Rename:"
    for s,d in renames:
        print "\t", s, "->", d
    print ""
    print "Will Create:"
    for d in dirs:
        print "\t", d
    print ""
    print "Will Install:"
    for s, d in installs:
        print "\t", s, "->", d
    print ""
    print "Will Execute:"
    for e in executes:
        print "\t", e
    print ""

    print "(C)onfirm, (A)bort ?"
    action = None
    while not(action in ['A', 'C']):
        action = sys.stdin.readline().strip()

    if action == 'A':
        return

    print "Removing ..."
    for r in removes:
        if path.isfile(r) or path.islink(r):
            os.remove(r)
        else:
            os.removedirs(r)

    print "Renaming ..."
    for s, d in renames:
        os.rename(s, d)

    print "Creating ..."
    for d in dirs:
        os.mkdir(d)

    print "Installing ..."
    for s, d in installs:
        os.symlink(s, os.path.abspath( d ))

    dot_path = os.path.dirname(  os.path.realpath(__file__) )
    new_env = dict( os.environ)
    new_env['DOT_PATH'] = dot_path
    for e in executes:
        print "Executing ", e
        subprocess.call([os.path.abspath("./"+e)],
                        cwd=os.path.expanduser("~"),
                        env=new_env)
        print "-----"

def find_all_dots():
    dots = {}
    # dots maps name => (path, install, bundle)
    #     if path is set, a link is created on home
    #     if bundle is set, then every file inside bundle/name is linked to home and following paths
    #     if install is set, the program will be executed afterwards

    install_re = re.compile(r'^INSTALL_(?P<dot>[a-z0-9][a-z0-9\.\-]+)\.[a-z0-9]+$')

    for dot in os.listdir('dot'):
        if not(install_re.match(dot)):
            add_dot(dots, dot, dot, None, None)

    for dot in os.listdir('install'):
        script = os.path.join('install', dot)
        add_dot(dots, dot, None, script, None)

    for dot in os.listdir('bundle'):
        add_dot(dots, dot, None, None, dot )

    for cur in ['dot', 'bundle', 'install']:
        l = os.listdir(cur)
        for dot in l:
            match = install_re.match(dot)
            if match:
                script = os.path.join(cur, dot)
                add_dot(dots, match.groupdict()['dot'], None, script, None)
    return dots

def main():
    dots = find_all_dots()
    if path.exists(SELECTION_FILE):
        selection = map(lambda l: l.strip() ,open(SELECTION_FILE).readlines())
    else:
        selection = dots.keys()

    print dots
    print "Selected dots:", "\n".join(map(lambda i: " - " + i, selection))

    print ""

    removes = []
    renames = []
    dirs = []
    installs = []
    executes = []

    from itertools import starmap
    for dot in selection:
        print "Installing dot '%s'" % dot
        tup = dot_install(dot, *dots[dot])
        map(lambda t: t[0].extend(t[1]),
                zip([ removes, renames, dirs, installs, executes ],
                    tup))

        print "---------------------------------"

    do_all_actions(removes, renames, dirs, installs, executes)

if __name__ == "__main__": main()
