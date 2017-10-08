# Copyright (C) 2013 Google Inc.
#
# This file is part of ycmd.
#
# ycmd is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# ycmd is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with ycmd.  If not, see <http://www.gnu.org/licenses/>.

from nose.tools import eq_
from ycmd.extra_conf_store import _PathsToAllParentFolders
import os.path


def PathsToAllParentFolders_Basic_test():
  eq_( [
    os.path.normpath( '/home/user/projects' ),
    os.path.normpath( '/home/user' ),
    os.path.normpath( '/home' ),
    os.path.normpath( '/' )
  ], list( _PathsToAllParentFolders( '/home/user/projects/test.c' ) ) )


def PathsToAllParentFolders_FileAtRoot_test():
  eq_( [ os.path.normpath( '/' ) ],
       list( _PathsToAllParentFolders( '/test.c' ) ) )


# We can't use backwards slashes in the paths because then the test would fail
# on Unix machines
def PathsToAllParentFolders_WindowsPath_test():
  eq_( [
    os.path.normpath( r'C:/foo/goo/zoo' ),
    os.path.normpath( r'C:/foo/goo' ),
    os.path.normpath( r'C:/foo' ),
    os.path.normpath( r'C:/' )
  ], list( _PathsToAllParentFolders( r'C:/foo/goo/zoo/test.c' ) ) )
