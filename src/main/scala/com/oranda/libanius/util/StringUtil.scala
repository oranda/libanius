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

package com.oranda.libanius.util

import java.lang.{Character, StringBuilder}

object StringUtil {

  def parseValue(str: String, beginStr: String, endStr: String): Option[String] = {
    val beginIndex = str.optionalIndex(beginStr)
    beginIndex.flatMap { case beginIndex =>
      val beginTextIndex: Int       = beginIndex + beginStr.length
      val endTextIndex: Option[Int] = str.optionalIndex(endStr, beginTextIndex + 1)
      endTextIndex.map(str.substring(beginTextIndex, _))
    }
  }

  def parseInt(str: String, beginStr: String, endStr: String): Option[Int] =
    parseValue(str, beginStr, endStr).map(_.toInt)

  /** The normal Scala mkString is too slow, so this is used instead.
   * It's necessary to pass in an existing StringBuilder.
   */
  def mkString[S <: StringBuilder, T](
    str: S,
    iterable: Iterable[T],
    fn: (S, T) => StringBuilder,
    separator: Character
  ): S = {
    val iter = iterable.iterator
    while iter.hasNext do {
      fn(str, iter.next)
      if iter.hasNext then str.append(separator)
    }
    str
  }

  def formatScore(score: BigDecimal): String = {
    val scoreStr         = (score * 100).toString
    val scoreStrMaxIndex = scala.math.min(scoreStr.length, 6)
    scoreStr.substring(0, scoreStrMaxIndex) + "%"
  }

  implicit class RichString(s: String) {

    def removeAll(substr: String): String = s.replaceAll(substr, "")

    def removeFirstLetters(substr: String): String =
      if s.startsWith(substr) then s.replaceFirst(substr, "") else s

    def optionalIndex(substr: String): Option[Int] = {
      val index = s.indexOf(substr)
      if index == -1 then None else Some(index)
    }

    def optionalIndex(substr: String, from: Int): Option[Int] = {
      val index = s.indexOf(substr, from)
      if index == -1 then None else Some(index)
    }
  }
}
