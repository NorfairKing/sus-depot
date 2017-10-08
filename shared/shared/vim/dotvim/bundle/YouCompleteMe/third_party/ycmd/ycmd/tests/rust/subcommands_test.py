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

from rust_handlers_test import Rust_Handlers_test
from nose.tools import eq_


class Rust_Subcommands_test( Rust_Handlers_test ):


  def _GoTo( self, params ):
    filepath = self._PathToTestFile( 'test.rs' )
    contents = open( filepath ).read()

    self._WaitUntilServerReady()

    command = params[ 'command' ]
    goto_data = self._BuildRequest( completer_target = 'filetype_default',
                                    command_arguments = [ command ],
                                    line_num = 7,
                                    column_num = 12,
                                    contents = contents,
                                    filetype = 'rust',
                                    filepath = filepath )

    results = self._app.post_json( '/run_completer_command',
                                   goto_data )

    eq_( {
      'line_num': 1, 'column_num': 8, 'filepath': filepath
    }, results.json )


  def GoTo_all_test( self ):
    tests = [
      { 'command': 'GoTo' },
      { 'command': 'GoToDefinition' },
      { 'command': 'GoToDeclaration' }
    ]

    for test in tests:
      yield ( self._GoTo, test )
