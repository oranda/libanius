/*
 * Libanius
 * Copyright (C) 2012-2016 James McCabe <james@oranda.com>
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

package com.oranda.libanius.model.quizitem

import scala.language.implicitConversions
import scala.util.matching.Regex
import com.oranda.libanius.dependencies.AppDependencyAccess
import com.oranda.libanius.util.StringUtil
import StringUtil._

case class TextValue(value: String) {

  override def toString = value

  def matches(otherText: String) = value == otherText

  /*
   * This is useful when the current value is a correct response with bracketed
   * expressions that are optional.
   */
  def looselyMatches(userResponse: String): Boolean =
    DesiredResponse(value).matches(userResponse)

  def hasSameStart(otherValue: String): Int => Boolean =
    TextValue.hasSameStart(value, otherValue)
  def hasSameEnd(otherValue: String): Int => Boolean =
    TextValue.hasSameEnd(value, otherValue)

}

object TextValue {

  def sameStart =
    (value1: TextValue, value2: TextValue) => hasSameStart(value1.value, value2.value)
  def sameEnd =
    (value1: TextValue, value2: TextValue) => hasSameEnd(value1.value, value2.value)

  def hasSameStart(value: String, otherValue: String): Int => Boolean =
    (numOfLetters: Int) => otherValue != value &&
      value.take(numOfLetters) == otherValue.take(numOfLetters)

  def hasSameEnd(value: String, otherValue: String): Int => Boolean =
    (numOfLetters: Int) => otherValue != value &&
      value.takeRight(numOfLetters) == otherValue.takeRight(numOfLetters)

  // Forward calls to String where necessary.
  implicit def textValue2String(textValue: TextValue): String = textValue.value
}

/*
 * A correct response with bracketed expressions that are optional.
 */
case class DesiredResponse(value: String) {
  import DesiredResponse._

  def matches(userResponse: String) =
    standardized(userResponse) == standardized(value)

  def matchesParts(userResponse: String) =
    userResponse == value ||
      containsPartsIn(standardized2(userResponse), standardized2(value).takeWhile(c => c != '/'))
}

object DesiredResponse extends AppDependencyAccess {

  def standardized(response: String) =
    response.toLowerCase.removeFirstLetters("to ").removeAll(" ").removeAll("-").
        removeAll(",").removeAll("\\.").removeAll("!").
        takeWhile(c => c != '/' && c != '(' && c != '[' && c != '{')

  def standardized1(response: String) =
    response.toLowerCase.removeFirstLetters("to ").removeAll(" ").removeAll("-")

  def standardized2(response: String) =
    response.removeAll(",").removeAll("\\.").removeAll("!").trim.toLowerCase

  def containsPartsIn(userResponse: String, desiredResponse: String): Boolean = {
    val correctResponseRequiredParts = partsOutsideBrackets(desiredResponse)
    allPartsArePresentAndInTheRightOrder(correctResponseRequiredParts, userResponse)
  }

  private def partsOutsideBrackets(desiredResponse: String): List[String] = {
    val pattern = new Regex("""([\s|\w]+)(\(|\z)""")
    val matchIter = pattern.findAllIn(desiredResponse)
    matchIter.matchData.toList map { m => m.subgroups(0).trim() }
  }

  private def allPartsArePresentAndInTheRightOrder(requiredParts: List[String],
      userResponse: String): Boolean = {
    val indexes = requiredParts.map(userResponse.indexOf)
    !indexes.contains(-1) && indexes.sortWith(_<_) == indexes
  }
}