# This file is NOT licensed under the GPLv3, which is the license for the rest
# of YouCompleteMe.
#
# Here's the license text for this file:
#
# This is free and unencumbered software released into the public domain.
#
# Anyone is free to copy, modify, publish, use, compile, sell, or
# distribute this software, either in source code form or as a compiled
# binary, for any purpose, commercial or non-commercial, and by any
# means.
#
# In jurisdictions that recognize copyright laws, the author or authors
# of this software dedicate any and all copyright interest in the
# software to the public domain. We make this dedication for the benefit
# of the public at large and to the detriment of our heirs and
# successors. We intend this dedication to be an overt act of
# relinquishment in perpetuity of all present and future rights to this
# software under copyright law.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
# IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
# OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
# ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
# OTHER DEALINGS IN THE SOFTWARE.
#
# For more information, please refer to <http://unlicense.org/>

import os
import os.path
import ycm_core
import subprocess

FLAG_NOT_FOUND = 0
FLAG_COMPILATION_DATABASE = 1
FLAG_CLANG_COMPLETE = 2


def dir_of(fname):
    return os.path.dirname( os.path.abspath(fname) )

def DirectoryOfThisScript():
    return dir_of(__file__)

def get_gpp_system_flags():
    """
    System include dirs sometimes are needed by clang and others
    """
    import locale
    encoding = locale.getdefaultlocale()[1]
    gpp_major = \
        subprocess.Popen("g++ -dumpversion".split(" "),
                         stdout=subprocess.PIPE).stdout.readlines()[0].decode(encoding).strip(' \n')
    gpp_machine =\
        subprocess.Popen("g++ -dumpmachine".split(" "),
                         stdout=subprocess.PIPE).stdout.readlines()[0].decode(encoding).strip(' \n')


    #  Ubuntu g++ 4.8 has includes in i386-linux-gnu, even though it swers it's
    #  i686 ...
    xtra_incs = " -I/usr/include/%s/c++/%s/" % (gpp_machine, gpp_major)
    if (gpp_machine == 'i686-linux-gnu'):
        xtra_incs = xtra_incs + \
                    " -I/usr/include/i386-linux-gnu/c++/%s/" % gpp_major

    return [f.strip() for f in xtra_incs.split(' ')]

def get_default_flags_for_file(fname):
    flags = []
    try:
        extension = fname.split('.')[-1]
        if extension in ['C', 'cpp', 'CPP', 'H', 'hpp', 'HPP', 'c++', 'C++']:
            flags.extend( ['-x', 'c++', '-std=c++14'] )

        return flags
    except:
        return [ '-x', 'c++', ]  # when in doubt, pretend it's c++

def dir_has_compilation_database(cur_dir):
    database_fname = os.path.join(cur_dir, 'compile_commands.json')
    if os.path.exists(database_fname):
        return database_fname

    return None

def dir_has_clang_complete(cur_dir):
    clang_complete_fname = os.path.join(cur_dir, '.clang_complete')

    if os.path.exists(clang_complete_fname):
        return clang_complete_fname

    return None

def get_compilation_hints_for_file(fname):
    """
    Find a compilation database or a .clang_complete file somewhere in the tree of the file
    """

    found_comp_db = False
    found_clang_complete = False

    cur_path = os.path.normpath(dir_of(fname))
    while cur_path != '/':
        # Check if we have some kind of flags for this project
        c = dir_has_compilation_database(cur_path)
        if c and not found_comp_db:
            found_comp_db = True
            yield (FLAG_COMPILATION_DATABASE, c)

        c = dir_has_clang_complete(cur_path)
        if c and not found_clang_complete:
            yield (FLAG_CLANG_COMPLETE, c)

        # still not found, look up the tree
        cur_path = os.path.normpath(os.path.join(cur_path, os.path.pardir))

    #return (FLAG_NOT_FOUND, None)

def get_clang_complete_flags(fname):
    '''
    Read flags from the .clang_complete if it exists
    '''
    flags = []
    try:
        clang_flags = open(fname, 'r').readlines()

        for fl in clang_flags:
            flags.extend([f.strip() for f in fl.split(" ") ] )

    except:
        pass
    return flags

def MakeRelativePathsInFlagsAbsolute( flags, working_directory ):
    """
    Make relative paths absolute

    Set this to the absolute path to the folder (NOT the file!) containing the
    compile_commands.json file to use that instead of 'flags'. See here for
    more details: http://clang.llvm.org/docs/JSONCompilationDatabase.html

    Most projects will NOT need to set this to anything; you can just change the
    'flags' list of compilation flags. Notice that YCM itself uses that approach.
    """
    if not working_directory:
        return list( flags )
    new_flags = []
    make_next_absolute = False
    path_flags = [ '-isystem', '-I', '-iquote', '--sysroot=' , '-include']
    for flag in flags:
        new_flag = flag

        if make_next_absolute:
            make_next_absolute = False
            if not flag.startswith( '/' ):
                new_flag = os.path.join( working_directory, flag )

        for path_flag in path_flags:
            if flag == path_flag:
                make_next_absolute = True
                break

            if flag.startswith( path_flag ):
                path = flag[ len( path_flag ): ]
                new_flag = path_flag + os.path.join( working_directory, path )
                break

        if new_flag:
            new_flags.append( new_flag )
    return new_flags


def FlagsForFile( filename ):
    """
    MAIN ENTRY POINT -- returns a dict containing flags for filename

    This tries to find either a compilation database OR a]
    .[clang_complete, ycm_complete] file to load flags from
    """

    flags = []
    def_flags = []
    database = None
    relative_to=None

    # Defaults
    def_flags.extend(get_default_flags_for_file(filename))
    def_flags.extend(get_gpp_system_flags())

    for kind, fname in get_compilation_hints_for_file(filename):
        if (kind == FLAG_CLANG_COMPLETE):
            # Load flags from the returned file
            print ("Loading clang complete file: %s" % fname)
            def_flags.extend(get_clang_complete_flags(fname))
            relative_to = dir_of(fname)
        elif (kind == FLAG_COMPILATION_DATABASE):
            # Load flags from the compilation database
            print ("Loading compilation database file: %s" % fname)
            compilation_database_folder = os.path.dirname(fname)
            database = ycm_core.CompilationDatabase( compilation_database_folder )

            if database:
                # Bear in mind that compilation_info.compiler_flags_ does NOT return a
                # python list, but a "list-like" StringVec object
                compilation_info = database.GetCompilationInfoForFile( filename )

                flags.extend(compilation_info.compiler_flags_)
                relative_to = compilation_info.compiler_working_dir_

                # NOTE: This is just for YouCompleteMe; it's highly likely that your project
                # does NOT need to remove the stdlib flag. DO NOT USE THIS IN YOUR
                # ycm_extra_conf IF YOU'RE NOT 100% YOU NEED IT.
                try:
                    flags.remove( '-stdlib=libc++' )
                except ValueError:
                    pass
            else:
                relative_to = dir_of(filename)

    flags.extend(def_flags)

    final_flags = MakeRelativePathsInFlagsAbsolute( flags, relative_to )

    return {
        'flags': final_flags,
        'do_cache': True
    }
