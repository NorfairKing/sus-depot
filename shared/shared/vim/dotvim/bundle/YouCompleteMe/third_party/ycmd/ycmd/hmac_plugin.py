# Copyright (C) 2014 Google Inc.
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

import logging
import httplib
from urlparse import urlparse
from base64 import b64decode, b64encode
from bottle import request, response, abort
from ycmd import hmac_utils

_HMAC_HEADER = 'x-ycm-hmac'
_HOST_HEADER = 'host'

# This class implements the Bottle plugin API:
# http://bottlepy.org/docs/dev/plugindev.html
#
# We want to ensure that every request coming in has a valid HMAC set in the
# x-ycm-hmac header and that every response coming out sets such a valid header.
# This is to prevent security issues with possible remote code execution.
# The x-ycm-hmac value is encoded as base64 during transport instead of sent raw
# because https://tools.ietf.org/html/rfc5987 says header values must be in the
# ISO-8859-1 character set.
class HmacPlugin( object ):
  name = 'hmac'
  api = 2


  def __init__( self, hmac_secret ):
    self._hmac_secret = hmac_secret
    self._logger = logging.getLogger( __name__ )


  def __call__( self, callback ):
    def wrapper( *args, **kwargs ):
      if not HostHeaderCorrect( request ):
        self._logger.info( 'Dropping request with bad Host header.' )
        abort( httplib.UNAUTHORIZED, 'Unauthorized, received bad Host header.' )
        return

      body = request.body.read()
      if not RequestAuthenticated( request.method, request.path, body,
                                   self._hmac_secret ):
        self._logger.info( 'Dropping request with bad HMAC.' )
        abort( httplib.UNAUTHORIZED, 'Unauthorized, received bad HMAC.' )
        return
      body = callback( *args, **kwargs )
      SetHmacHeader( body, self._hmac_secret )
      return body
    return wrapper


def HostHeaderCorrect( request ):
  host = urlparse( 'http://' + request.headers[ _HOST_HEADER ] ).hostname
  return host == '127.0.0.1' or host == 'localhost'


def RequestAuthenticated( method, path, body, hmac_secret ):
  if _HMAC_HEADER not in request.headers:
    return False

  return hmac_utils.SecureStringsEqual(
      hmac_utils.CreateRequestHmac( method, path, body, hmac_secret ),
      b64decode( request.headers[ _HMAC_HEADER ] ) )


def SetHmacHeader( body, hmac_secret ):
  response.headers[ _HMAC_HEADER ] = b64encode(
      hmac_utils.CreateHmac( body, hmac_secret ) )
