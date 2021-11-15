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

# database : CompilationDatabase
# obtained from ycm
database = None
database_path = None

# db_json : [dict]
# obtained by parsing the compilation database
db_json = None

try:
    from subprocess import DEVNULL # py3k
except ImportError:
    import os
    DEVNULL = open(os.devnull, 'wb')

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

def get_default_flags_for_file(fname, cur_flags):
    flags = []
    extension = os.path.splitext(fname)[1]
    if extension in CPP_SOURCES:
        # print 'cpp file'
        # no problem setting c++ twice, but setting incompabible standards may
        # lead to unintended consequences
        flags.extend( ['-x', 'c++'] )
        if not any(map(lambda f: f.startswith('-std='), cur_flags)):
            flags.append('-std=c++17')

    # print 'for file', fname, 'ext', extension, 'flags:', flags

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

def find_compilation_database_from(cur_path):
    """
    Find a compilation database file somewhere in the tree of the file
    """

    while cur_path != '/':
        if dir_has_compilation_database(cur_path):
            return cur_path

        # still not found, look up the tree
        # normalize(par/cur/../) -> par/
        cur_path = os.path.normpath(os.path.join(cur_path, os.path.pardir))

    return None

def find_clang_completion_file_from(cur_path):
    """
    Find a compilation .clang_complete file somewhere in the tree of the file
    """

    while cur_path != '/':
        if dir_has_clang_complete(cur_path):
            return os.path.join(cur_path, '.clang_complete')

        # still not found, look up the tree
        cur_path = os.path.normpath(os.path.join(cur_path, os.path.pardir))

    return None

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

# From YCM own .ycm_extra_conf
SYSTEM_INCLUDE_PATHS=[
    "/usr/include",
    "/usr/local/include"
    # what more?
]
def is_system_include(header_path):
    for system_path in SYSTEM_INCLUDE_PATHS:
        if os.path.commonprefix([system_path, header_path]) == system_path:
            return True

    return False

SOURCE_EXTENSIONS = [ '.cpp', '.cxx', '.cc', '.c', '.m', '.mm', '.C', '.Cpp', '.CPP' ]
CPP_SOURCES = [ '.cpp', '.cxx', '.cc', '.C', '.Cpp', '.CPP', 'c++', 'C++' ]
HEADER_EXTENSIONS = [ '.h', '.hxx', '.hpp', '.hh']
def IsHeaderFile( filename ):
  extension = os.path.splitext( filename )[ 1 ]
  return extension.lower() in HEADER_EXTENSIONS

def extend_flags_with_defaults(filename, flags, relative_to):
    def_flags = []
    def_flags.extend(get_default_flags_for_file(filename, flags))
    def_flags.extend(get_gpp_system_flags())

    # print "adding", def_flags

    final_flags = MakeRelativePathsInFlagsAbsolute( flags + def_flags, relative_to )

    return final_flags

def is_subdir_of_any(header_path, includes):
    for include in includes:
        if os.path.commonprefix([header_path, include]) == include:
            return True

        return False

def files_including_header(header_full_path, all=False):
    global database_path, db_json
    import json
    import subprocess

    if not database_path:
        print ("oops, missing database path")
        return []

    # cache json dict
    if not db_json:
        # header_name = os.path.splitext(os.path.basename(header_full_oath))[0]

        database_file = dir_has_compilation_database(database_path)
        if not database_file:
            print ("wtf no database file")

        db_json = json.load(file(database_file, 'r'))

        #print "loaded raw database from ", database_file

    files = []
    header_full_path = os.path.normpath(header_full_path)
    #print "looking for ", header_full_path
    try:
        # see if any known file includes header
        for entry in db_json:
            cmd = entry['command']
            entry_file = entry['file']
            # print "checking ", entry_file
            entry_path = entry['directory']

            def de_include(i):
                i = i[2:]

                if not os.path.isabs(i):
                    i = os.path.abspath(os.path.join(entry_path, i))

                return i

            entry_includes = [ i for i in cmd.split(' ') if i.startswith('-I') ]
            entry_include_dirs = map(de_include, entry_includes)

            #cpp -Iinclude -I. -I../Debug/StackVM/generated -I ../Debug/deps/antlrruntime/install/include/antlr4-runtime -H stackvm.cpp -o /dev/null 2>&1|less

            cmd = ['cpp', '-H']
            cmd.extend(entry_includes)
            cmd.append(entry_file)
            #print "Executing ", ' '.join(cmd)

            #print 'cpp for ', entry_file
            # cpp sends output to dev null, includes to err
            p = subprocess.Popen(cmd, cwd=entry_path,
                                 stderr=subprocess.PIPE,
                                 stdin=DEVNULL , stdout=DEVNULL)
            for inc in p.stderr:
                # output is :
                # . include/vm.H
                # .. /usr/include/stdio.h
                # print 'inc is', inc
                inc = inc.strip()
                if inc.startswith('.'):
                    try:
                        include_file = ' '.join(inc.split(' ')[1:])

                        # make sure we have abs path
                        if not os.path.isabs(include_file):
                            include_file = os.path.join(entry_path, include_dir)

                        include_path = os.path.normpath(include_file)

                        # if, once normalized and absolute, they are the same file,
                        # then this entry includes the header
                        if include_file == header_full_path:
                            files.append(entry_file)
                            if not all:
                                break

                            # if all, just stop reading this file. GO on to the next
                            break
                    except Exception as e:
                        print ("oops", e)
                    pass # probably output included something strange
                pass # if inc.startswith
            pass # for inc in stderr

            p.kill()

            if not all and len(files) > 0:
                break # break  entry loop
        pass # for entry in json
    except Exception as e:
        import traceback
        print ("Could not load database ", database_path, ": ", e)
        traceback.print_exc()
        return []

    return files

def get_compilation_hints_for_file(file_full_path):
    global database_path, database
    cur_path = os.path.normpath(dir_of(file_full_path))
    flags = []
    def_flags = []

    # print "Flags for File", file_full_path

    # no compilation flags for system includes
    if IsHeaderFile(file_full_path) and is_system_include(file_full_path):
        relative_to = dir_of(file_full_path)
        return {
            'flags': extend_flags_with_defaults(file_full_path, [], relative_to),
            'do_cache': True
        }

    # First, try the "normal" one: A file is either
    # - C/C++ file, and thus should be in the compilation database
    # - A header, which usually has an associated implementation. See if this c file
    #   has compilation flags
    if not(database):
        database_path = find_compilation_database_from(cur_path)
        if database_path:
            print ("Loading compilation database file: %s" % database_path)
            database = ycm_core.CompilationDatabase( database_path )

    if database:
        print ("Trying database")

        database = ycm_core.CompilationDatabase( database_path )

        info = database.GetCompilationInfoForFile( file_full_path )

        if info.compiler_flags_:
            # Found compilation info on database
            print ("Found info on database: ", ' '.join(info.compiler_flags_))
            flags.extend(info.compiler_flags_)
            relative_to = info.compiler_working_dir_
            return {
                'flags': extend_flags_with_defaults(file_full_path, flags, relative_to),
                'do_cache': True
            }

        print ("No info for file on database")
        # file not in database. If it's a header, try to find it's
        # implementation
        if IsHeaderFile(file_full_path):
            basename = os.path.splitext( file_full_path )[ 0 ]
            for extension in SOURCE_EXTENSIONS:
                replacement_file = basename + extension
                print ("Trying replacement", replacement_file)
                if os.path.exists( replacement_file ):
                    info = database.GetCompilationInfoForFile(replacement_file )
                if info.compiler_flags_:
                    print ("Found info for replacement on database")
                    flags.extend(info.compiler_flags_)
                    relative_to = info.compiler_working_dir_
                    return {
                        'flags': extend_flags_with_defaults(file_full_path, flags, relative_to),
                        'do_cache': True
                    }

    print ("No info on database")
    # No luck in the database. Try to find via .clang_flags file
    clang_complete_file = find_clang_completion_file_from(cur_path)
    if clang_complete_file:
        print ("Loading clang complete file in: %s" % clang_complete_file)
        flags.extend(get_clang_complete_flags(clang_complete_file))
        relative_to = dir_of(clang_complete_file)
        return {
            'flags': extend_flags_with_defaults(file_full_path, flags, relative_to),
            'do_cache': True
        }

    print ("No clang complete")

    # No entry in the database, no corresponding implementation. Now we try to find files that include the header
    if IsHeaderFile(file_full_path):
        print ("Going nuclear")
        # Could not find it in the compilation database. Now we go full oger mode
        files = files_including_header(file_full_path, all=False)
        if len(files) > 0:
            print ("Found include in ", files[0])
            info = database.GetCompilationInfoForFile( files[0] )
            flags.extend(info.compiler_flags_)
            relative_to = info.compiler_working_dir_
            return {
                'flags': extend_flags_with_defaults(file_full_path, flags, relative_to),
                'do_cache': True
            }

    # Nothing found, use defaults

    print ("defaults")
    relative_to = dir_of(file_full_path)
    return {
        'flags': extend_flags_with_defaults(file_full_path, [], relative_to),
        'do_cache': True
    }

# def get_compilation_hints_for_file(fname):
#     """
#     Find a compilation database or a .clang_complete file somewhere in the tree of the file
#     """

#     found_comp_db = False
#     found_clang_complete = False

#     cur_path = os.path.normpath(dir_of(fname))
#     while cur_path != '/':
#         # Check if we have some kind of flags for this project
#         c = dir_has_compilation_database(cur_path)
#         if c and not found_comp_db:
#             found_comp_db = True
#             yield (FLAG_COMPILATION_DATABASE, c)

#         c = dir_has_clang_complete(cur_path)
#         if c and not found_clang_complete:
#             yield (FLAG_CLANG_COMPLETE, c)

#         # still not found, look up the tree
#         cur_path = os.path.normpath(os.path.join(cur_path, os.path.pardir))

#     #return (FLAG_NOT_FOUND, None)

def FlagsForFile( filename ):
    """
    MAIN ENTRY POINT -- returns a dict containing flags for filename

    """

    print ("Asking flags for", filename)

    return get_compilation_hints_for_file( filename )

    # flags = []
    # def_flags = []
    # database = None
    # relative_to=None

    # # Defaults
    # def_flags.extend(get_default_flags_for_file(filename))
    # def_flags.extend(get_gpp_system_flags())

    # for kind, fname in get_compilation_hints_for_file(filename):
    #     if (kind == FLAG_CLANG_COMPLETE):
    #         # Load flags from the returned file
    #         print ("Loading clang complete file: %s" % fname)
    #         def_flags.extend(get_clang_complete_flags(fname))
    #         relative_to = dir_of(fname)
    #     elif (kind == FLAG_COMPILATION_DATABASE):
    #         # Load flags from the compilation database
    #         print ("Loading compilation database file: %s" % fname)
    #         compilation_database_folder = os.path.dirname(fname)
    #         database = ycm_core.CompilationDatabase( compilation_database_folder )

    #         if database:
    #             # Bear in mind that compilation_info.compiler_flags_ does NOT return a
    #             # python list, but a "list-like" StringVec object
    #             compilation_info = database.GetCompilationInfoForFile( filename )

    #             flags.extend(compilation_info.compiler_flags_)
    #             relative_to = compilation_info.compiler_working_dir_

    #             # NOTE: This is just for YouCompleteMe; it's highly likely that your project
    #             # does NOT need to remove the stdlib flag. DO NOT USE THIS IN YOUR
    #             # ycm_extra_conf IF YOU'RE NOT 100% YOU NEED IT.
    #             try:
    #                 flags.remove( '-stdlib=libc++' )
    #             except ValueError:
    #                 pass
    #         else:
    #             relative_to = dir_of(filename)

    # flags.extend(def_flags)

    # final_flags = MakeRelativePathsInFlagsAbsolute( flags, relative_to )

    # return {
    #     'flags': final_flags,
    #     'do_cache': True
    # }
