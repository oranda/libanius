/*
 * Libanius
 * Copyright (C) 2012-2019 James McCabe <james@oranda.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.oranda.libanius.util

import java.lang.Character

abstract class StringSplitter(separator: Character) extends Iterator[String] {
  override def hasNext: Boolean

  override def next: String
  def setString(str: String)
}

/*
 * Created to have a similar interface to the Android splitter, but done in
 * regular Scala/Java. This is used on a PC and for unit tests.
 */
class StringSplitterDefault(separator: Character) extends StringSplitter(separator) {

  private var iter: Iterator[String] = _

  override def setString(str: String): Unit = {
    iter = str.split("\\" + separator.toString).filter(_ != "").iterator
  }

  override def hasNext: Boolean = iter.hasNext

  override def next: String = iter.next
}
