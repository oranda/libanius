/*
 * Copyright 2012 James McCabe <james@oranda.com>
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may
 * not use this file except in compliance with the License. You may obtain
 * a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.oranda.libanius.util

import java.lang.Character

object StringUtil {
    
  def parseValue(str: String, beginStr: String, endStr: String) = {
    val beginIndex = str.indexOf(beginStr) + beginStr.length
    val endIndex = str.indexOf(endStr, beginIndex + 1)
    str.substring(beginIndex, endIndex)
  }
  
  /*
   * The normal Scala mkString is too slow, so this is used instead.
   * It's necessary to pass in an existing StringBuilder.
   */
  def mkString[S <: StringBuilder, T](str: S, iterable: Iterable[T], 
      fn: (S, T) => StringBuilder, separator: Character): S = {
    val iter = iterable.iterator
    while (iter.hasNext) {
      fn(str, iter.next)
      if (iter.hasNext)
        str.append(separator)
    }
    str
  }
}