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

import os

YCM_EXTRA_CONF_FILENAME = '.ycm_extra_conf.py'

CONFIRM_CONF_FILE_MESSAGE = ('Found {0}. Load? \n\n(Question can be turned '
                             'off with options, see YCM docs)')

NO_EXTRA_CONF_FILENAME_MESSAGE = ( 'No {0} file detected, so no compile flags '
  'are available. Thus no semantic support for C/C++/ObjC/ObjC++. Go READ THE '
  'DOCS *NOW*, DON\'T file a bug report.' ).format( YCM_EXTRA_CONF_FILENAME )

NO_DIAGNOSTIC_SUPPORT_MESSAGE = ( 'YCM has no diagnostics support for this '
  'filetype; refer to Syntastic docs if using Syntastic.')


class ServerError( Exception ):
  def __init__( self, message ):
    super( ServerError, self ).__init__( message )


class UnknownExtraConf( ServerError ):
  def __init__( self, extra_conf_file ):
    message = CONFIRM_CONF_FILE_MESSAGE.format( extra_conf_file )
    super( UnknownExtraConf, self ).__init__( message )
    self.extra_conf_file = extra_conf_file


class NoExtraConfDetected( ServerError ):
  def __init__( self ):
    super( NoExtraConfDetected, self ).__init__(
      NO_EXTRA_CONF_FILENAME_MESSAGE )


class NoDiagnosticSupport( ServerError ):
  def __init__( self ):
    super( NoDiagnosticSupport, self ).__init__( NO_DIAGNOSTIC_SUPPORT_MESSAGE )


def BuildGoToResponse( filepath, line_num, column_num, description = None ):
  response = {
    'filepath': os.path.realpath( filepath ),
    'line_num': line_num,
    'column_num': column_num
  }

  if description:
    response[ 'description' ] = description
  return response


def BuildDescriptionOnlyGoToResponse( text ):
  return {
    'description': text,
  }


def BuildDisplayMessageResponse( text ):
  return {
    'message': text
  }


def BuildDetailedInfoResponse( text ):
  """ Retuns the response object for displaying detailed information about types
  and usage, suach as within a preview window"""
  return {
    'detailed_info': text
  }


def BuildCompletionData( insertion_text,
                         extra_menu_info = None,
                         detailed_info = None,
                         menu_text = None,
                         kind = None,
                         extra_data = None ):
  completion_data = {
    'insertion_text': insertion_text
  }

  if extra_menu_info:
    completion_data[ 'extra_menu_info' ] = extra_menu_info
  if menu_text:
    completion_data[ 'menu_text' ] = menu_text
  if detailed_info:
    completion_data[ 'detailed_info' ] = detailed_info
  if kind:
    completion_data[ 'kind' ] = kind
  if extra_data:
    completion_data[ 'extra_data' ] = extra_data
  return completion_data


def BuildCompletionResponse( completion_datas,
                             start_column,
                             errors=None ):
  return {
    'completions': completion_datas,
    'completion_start_column': start_column,
    'errors': errors if errors else [],
  }


def BuildLocationData( location ):
  return {
    'line_num': location.line_number_,
    'column_num': location.column_number_,
    'filepath': location.filename_,
  }


def BuildRangeData( source_range ):
  return {
    'start': BuildLocationData( source_range.start_ ),
    'end': BuildLocationData( source_range.end_ ),
  }


class Diagnostic:
  def __init__ ( self, ranges, location, location_extent, text, kind ):
    self.ranges_ = ranges
    self.location_ = location
    self.location_extent_ = location_extent
    self.text_ = text
    self.kind_ = kind


class FixIt:
  """A set of replacements (of type FixItChunk) to be applied to fix a single
  diagnostic. This can be used for any type of refactoring command, not just
  quick fixes. The individual chunks may span multiple files."""

  def __init__ ( self, location, chunks ):
    """location of type Location, chunks of type list<FixItChunk>"""
    self.location = location
    self.chunks = chunks


class FixItChunk:
  """An individual replacement within a FixIt (aka Refactor)"""

  def __init__ ( self, replacement_text, range ):
    """replacement_text of type string, range of type Range"""
    self.replacement_text = replacement_text
    self.range = range


class Range:
  """Source code range relating to a diagnostic or FixIt (aka Refactor)."""

  def __init__ ( self, start, end ):
    "start of type Location, end of type Location"""
    self.start_ = start
    self.end_ = end


class Location:
  """Source code location for a diagnostic or FixIt (aka Refactor)."""

  def __init__ ( self, line, column, filename ):
    """Line is 1-based line, column is 1-based column, filename is absolute
    path of the file"""
    self.line_number_ = line
    self.column_number_ = column
    self.filename_ = filename


def BuildDiagnosticData( diagnostic ):

  kind = ( diagnostic.kind_.name if hasattr( diagnostic.kind_, 'name' )
           else diagnostic.kind_ )

  fixits = ( diagnostic.fixits_ if hasattr( diagnostic, 'fixits_' ) else [] )

  return {
    'ranges': [ BuildRangeData( x ) for x in diagnostic.ranges_ ],
    'location': BuildLocationData( diagnostic.location_ ),
    'location_extent': BuildRangeData( diagnostic.location_extent_ ),
    'text': diagnostic.text_,
    'kind': kind,
    'fixit_available': len( fixits ) > 0,
  }


def BuildFixItResponse( fixits ):
  """Build a response from a list of FixIt (aka Refactor) objects. This response
  can be used to apply arbitrary changes to arbitrary files and is suitable for
  both quick fix and refactor operations"""

  def BuildFixitChunkData( chunk ):
    return {
      'replacement_text': chunk.replacement_text,
      'range': BuildRangeData( chunk.range ),
    }

  def BuildFixItData( fixit ):
    return {
      'location': BuildLocationData( fixit.location ),
      'chunks' : [ BuildFixitChunkData( x ) for x in fixit.chunks ],
    }

  return {
    'fixits' : [ BuildFixItData( x ) for x in fixits ]
  }


def BuildExceptionResponse( exception, traceback ):
  return {
    'exception': exception,
    'message': str( exception ),
    'traceback': traceback
  }
