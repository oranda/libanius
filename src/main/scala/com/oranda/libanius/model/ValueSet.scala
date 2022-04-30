/*
 * Libanius
 * Copyright (C) 2012-2022 James McCabe <jjtmccabe@gmail.com>
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

package com.oranda.libanius.model

/*
 * Used for search results.
 */
case class ValueSet(values: List[String] = Nil)
  extends ModelComponent {

  def addValueToEnd(value: String) =
    copy(if (!values.contains(value)) values :+ value else values)

  override def toString = values.mkString(", ")

  def strings: Iterable[String] = values

  def size = values.size

  def containsValue(value: String): Boolean = values.contains(value)

  def valueBeginningWith(valueStart: String) = values.find(_.startsWith(valueStart))
}

