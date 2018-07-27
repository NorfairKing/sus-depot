# Copyright (C) 2015 ycmd contributors
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

from ..handlers_test import Handlers_test


class Typescript_Handlers_test( Handlers_test ):

  def __init__( self ):
    self._file = __file__


  def CompletionEntryMatcher( self, insertion_text, menu_text = None ):
    if not menu_text:
      menu_text = insertion_text

    extra_params = { 'menu_text': menu_text }
    return self._CompletionEntryMatcher( insertion_text,
                                         extra_params = extra_params )
