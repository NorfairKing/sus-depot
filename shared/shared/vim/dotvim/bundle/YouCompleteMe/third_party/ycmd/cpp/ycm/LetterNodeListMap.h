// Copyright (C) 2011, 2012 Google Inc.
//
// This file is part of ycmd.
//
// ycmd is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// ycmd is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with ycmd.  If not, see <http://www.gnu.org/licenses/>.

#ifndef LETTERNODELISTMAP_H_BRK2UMC1
#define LETTERNODELISTMAP_H_BRK2UMC1

#include "DLLDefines.h"

#include <list>
#include <boost/utility.hpp>
#include <boost/array.hpp>

#define NUM_LETTERS 128

namespace YouCompleteMe {

class LetterNode;

YCM_DLL_EXPORT int IndexForChar( char letter );
YCM_DLL_EXPORT bool IsUppercase( char letter );

class LetterNodeListMap : boost::noncopyable {
public:
  LetterNodeListMap();
  YCM_DLL_EXPORT ~LetterNodeListMap();

  bool HasLetter( char letter );

  std::list< LetterNode * > &operator[] ( char letter );

  std::list< LetterNode * > *ListPointerAt( char letter );

  bool HasLetter( char letter ) const;

private:
  boost::array< std::list< LetterNode * >*, NUM_LETTERS > letters_;
};

} // namespace YouCompleteMe

#endif /* end of include guard: LETTERNODELISTMAP_H_BRK2UMC1 */

