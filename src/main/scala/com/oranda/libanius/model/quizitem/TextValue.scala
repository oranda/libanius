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

package com.oranda.libanius.model.quizitem

import scala.util.matching.Regex
import com.oranda.libanius.dependencies.AppDependencyAccess
import com.oranda.libanius.util.StringUtil
import StringUtil._

case class TextValue(override val value: String) extends Value[String](value) {

  override def toString = value

  def matches(otherText: String) = value == otherText

  /*
   * This is useful when the current value is a correct response with bracketed
   * expressions that are optional.
   */
  def looselyMatches(userResponse: String): Boolean =
    DesiredResponse(value).matches(userResponse)

  override def hasSameStart(otherValue: String): Int => Boolean =
    TextValue.hasSameStart(value, otherValue)
  override def hasSameEnd(otherValue: String): Int => Boolean =
    TextValue.hasSameEnd(value, otherValue)

}

object TextValue {
  def hasSameStart(value: String, otherValue: String): Int => Boolean =
    (numOfLetters: Int) => otherValue != value &&
        value.take(numOfLetters) == otherValue.take(numOfLetters)

  def hasSameEnd(value: String, otherValue: String): Int => Boolean =
    (numOfLetters: Int) => otherValue != value &&
        value.takeRight(numOfLetters) == otherValue.takeRight(numOfLetters)
}


/*
 * A correct response with bracketed expressions that are optional.
 */
case class DesiredResponse(value: String) {
  import DesiredResponse._

  def matches(userResponse: String) =
    userResponse.removeAll(" ") == value.removeAll(" ")

  //userResponse == value ||
  //containsPartsIn(standardized(userResponse), standardized(value).takeWhile(c => c != '/'))
}

object DesiredResponse extends AppDependencyAccess {

  def standardized(response: String) =
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
    val indexes = requiredParts.map(userResponse.indexOf(_))
    !indexes.contains(-1) && indexes.sortWith(_<_) == indexes
  }
}