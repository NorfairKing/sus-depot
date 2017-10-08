#!/usr/bin/env python

import os
import subprocess
import os.path as p
import sys

DIR_OF_THIS_SCRIPT = p.dirname( p.abspath( __file__ ) )
DIR_OF_THIRD_PARTY = p.join( DIR_OF_THIS_SCRIPT, 'third_party' )

python_path = []
for folder in os.listdir( DIR_OF_THIRD_PARTY ):
  python_path.append( p.abspath( p.join( DIR_OF_THIRD_PARTY, folder ) ) )
if os.environ.get( 'PYTHONPATH' ) is not None:
  python_path.append( os.environ['PYTHONPATH'] )
os.environ[ 'PYTHONPATH' ] = os.pathsep.join( python_path )

sys.path.insert( 1, p.abspath( p.join( DIR_OF_THIRD_PARTY, 'argparse' ) ) )

import argparse


def RunFlake8():
  print( 'Running flake8' )
  subprocess.check_call( [
    'flake8',
    '--select=F,C9',
    '--max-complexity=10',
    '--exclude=testdata',
    p.join( DIR_OF_THIS_SCRIPT, 'ycmd' )
  ] )


COMPLETERS = {
  'cfamily': {
    'build': [ '--clang-completer' ],
    'test': [ '--exclude-dir=ycmd/tests/clang' ],
    'aliases': [ 'c', 'cpp', 'c++', 'objc', 'clang', ]
  },
  'cs': {
    'build': [ '--omnisharp-completer' ],
    'test': [ '--exclude-dir=ycmd/tests/cs' ],
    'aliases': [ 'omnisharp', 'csharp', 'c#' ]
  },
  'javascript': {
    'build': [ '--tern-completer' ],
    'test': [ '--exclude-dir=ycmd/tests/javascript' ],
    'aliases': [ 'js', 'tern' ]
  },
  'go': {
    'build': [ '--gocode-completer' ],
    'test': [ '--exclude-dir=ycmd/tests/go' ],
    'aliases': [ 'gocode' ]
  },
  'rust': {
    'build': [ '--racer-completer' ],
    'test': [ '--exclude-dir=ycmd/tests/rust' ],
    'aliases': [ 'racer', 'racerd', ]
  },
  'typescript': {
    'build': [],
    'test': [ '--exclude-dir=ycmd/tests/typescript' ],
    'aliases': []
  },
  'python': {
    'build': [],
    'test': [ '--exclude-dir=ycmd/tests/python' ],
    'aliases': [ 'jedi', 'jedihttp', ]
  },
}


def CompleterType( value ):
  value = value.lower()
  if value in COMPLETERS:
    return value
  else:
    aliases_to_completer = dict( (i,k) for k,v in COMPLETERS.iteritems()
                                          for i in v[ 'aliases' ] )
    if value in aliases_to_completer:
      return aliases_to_completer[ value ];
    else:
      raise argparse.ArgumentTypeError(
        '{0} is not a valid completer - should be one of {1}'.format(
          value, COMPLETERS.keys() ) )


def ParseArguments():
  parser = argparse.ArgumentParser()
  group = parser.add_mutually_exclusive_group()
  group.add_argument( '--no-clang-completer', action = 'store_true',
                       help = argparse.SUPPRESS ) # deprecated 
  group.add_argument( '--no-completers', nargs ='*', type = CompleterType,
                       help = 'Do not build or test with listed semantic '
                       'completion engine(s). Valid values: {0}'.format(
                        COMPLETERS.keys()) )
  group.add_argument( '--completers', nargs ='*', type = CompleterType,
                       help = 'Only build and test with listed semantic '
                       'completion engine(s). Valid values: {0}'.format(
                        COMPLETERS.keys()) )
  parser.add_argument( '--skip-build', action = 'store_true',
                       help = 'Do not build ycmd before testing.' )
  parser.add_argument( '--msvc', type = int, choices = [ 11, 12, 14 ],
                       help = 'Choose the Microsoft Visual '
                       'Studio version. (default: 14).' )
  parser.add_argument( '--arch', type = int, choices = [ 32, 64 ],
                       help = 'Force architecture to 32 or 64 bits on '
                       'Windows (default: python interpreter architecture).' )
  parser.add_argument( '--coverage', action = 'store_true',
                       help = 'Enable coverage report (requires coverage pkg)' )
  parser.add_argument( '--no-flake8', action = 'store_true',
                       help = 'Disable flake8 run.' )

  parsed_args, nosetests_args = parser.parse_known_args()

  parsed_args.completers = FixupCompleters( parsed_args )

  if 'COVERAGE' in os.environ:
    parsed_args.coverage = ( os.environ[ 'COVERAGE' ] == 'true' )

  return parsed_args, nosetests_args


def FixupCompleters( parsed_args ):
  completers = set( COMPLETERS.keys() )
  if parsed_args.completers is not None:
    completers = set( parsed_args.completers )
  elif parsed_args.no_completers is not None:
    completers = completers.difference( parsed_args.no_completers )
  elif parsed_args.no_clang_completer:
    print( 'WARNING: The "--no-clang-completer" flag is deprecated. '
           'Please use "--no-completer cfamily" instead.' )
    completers.remove( 'cfamily' )

  if 'USE_CLANG_COMPLETER' in os.environ:
    if os.environ[ 'USE_CLANG_COMPLETER' ] == 'false':
      completers.remove( 'cfamily' )
    else:
      completers.add( 'cfamily' )

  return list( completers )


def BuildYcmdLibs( args ):
  if not args.skip_build:
    extra_cmake_args = [ '-DUSE_DEV_FLAGS=ON' ]

    os.environ[ 'EXTRA_CMAKE_ARGS' ] = ' '.join(extra_cmake_args)
    os.environ[ 'YCM_TESTRUN' ] = '1'

    build_cmd = [
      sys.executable,
      p.join( DIR_OF_THIS_SCRIPT, 'build.py' ),
    ]

    for key in COMPLETERS:
      if key in args.completers:
        build_cmd.extend( COMPLETERS[ key ][ 'build' ] )

    if args.msvc:
      build_cmd.extend( [ '--msvc', str( args.msvc ) ] )

    if args.arch:
      build_cmd.extend( [ '--arch', str( args.arch ) ] )

    subprocess.check_call( build_cmd )


def NoseTests( parsed_args, extra_nosetests_args ):
  # Always passing --with-id to nosetests enables non-surprising usage of
  # its --failed flag.
  nosetests_args = [ '-v', '--with-id' ]

  for key in COMPLETERS:
    if key not in parsed_args.completers:
      nosetests_args.extend( COMPLETERS[ key ][ 'test' ] )

  if parsed_args.coverage:
    nosetests_args += [ '--with-coverage', '--cover-package=ycmd' ]

  if extra_nosetests_args:
    nosetests_args.extend( extra_nosetests_args )
  else:
    nosetests_args.append( p.join( DIR_OF_THIS_SCRIPT, 'ycmd' ) )

  subprocess.check_call( [ 'nosetests' ] + nosetests_args )


def Main():
  parsed_args, nosetests_args = ParseArguments()
  if not parsed_args.no_flake8:
    RunFlake8()
  BuildYcmdLibs( parsed_args )
  NoseTests( parsed_args, nosetests_args )

if __name__ == "__main__":
  Main()
