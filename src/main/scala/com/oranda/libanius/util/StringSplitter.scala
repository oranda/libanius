/*
 * Copyright 2012-2013 James McCabe <james@oranda.com>
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

abstract class StringSplitter(char: Character) extends Iterator[String] {
  override def hasNext: Boolean 
  override def next: String 
  def setString(str: String)
}

/*
 * Created to have a similar interface to the Android splitter, but done in
 * regular Scala/Java. This is used on a PC and for unit tests.
 */
class StringSplitterDefault(_char: Character) extends StringSplitter(_char) {

  private var iter: Iterator[String] = _
  
  override def setString(str: String) {
    iter = str.split("\\" +_char.toString).filter(_ != "").iterator
  }
  
  override def hasNext: Boolean = iter.hasNext
  override def next: String = iter.next  
}