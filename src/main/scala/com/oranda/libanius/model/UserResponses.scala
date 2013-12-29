/*
 * Libanius
 * Copyright (C) 2012-2014 James McCabe <james@oranda.com>
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

import com.oranda.libanius.util.StringUtil

import java.lang.StringBuilder

import scalaz._

case class UserResponses(correctResponsesInARow: List[UserResponse] = Nil,
    incorrectResponses: List[UserResponse] = Nil) extends ModelComponent {

  def updated(value: String, correctResponsesInARow: List[UserResponse],
      incorrectResponses: List[UserResponse]): UserResponses =
    new UserResponses(correctResponsesInARow, incorrectResponses)

  def userResponses = correctResponsesInARow ++ incorrectResponses

  def add(userResponse: UserResponse, wasCorrect: Boolean): UserResponses =
    if (wasCorrect)
      UserResponses.userResponsesCorrectResponsesLens.set(this,
          userResponse :: correctResponsesInARow)
    else {
      // on an incorrect answer, old correct answers are discarded
      val ur = UserResponses.userResponsesCorrectResponsesLens.set(this, Nil)
      UserResponses.userResponsesIncorrectResponsesLens.set(ur, userResponse :: incorrectResponses)
    }

  /*
   * See if this quiz item meets any of the defined criteria sets that would make it presentable.
   * (Intended to be called over many quiz items until one fits.)
   */
  def isPresentable(currentPromptNum : Int): Boolean =
    isPresentable(currentPromptNum, promptNumInMostRecentResponse, numCorrectResponsesInARow)

  protected[model] def isPresentable(currentPromptNum : Int,
      promptNumInMostRecentResponse: Option[Int], numCorrectResponsesInARow: Int): Boolean =
    numCorrectResponsesInARow == 0 || Criteria.criteriaSets.exists(_.isPresentable(
        currentPromptNum, promptNumInMostRecentResponse, numCorrectResponsesInARow))

  def isUnfinished: Boolean = numCorrectResponsesInARow < conf.numCorrectAnswersRequired
  
  def numCorrectResponsesInARow = correctResponsesInARow.length
  
  def promptNumInMostRecentResponse: Option[Int] =
    correctResponsesInARow.headOption.orElse(incorrectResponses.headOption).orElse(None).
        map(_.promptNumber)

  def updated(correctResponsesInARow: List[UserResponse], incorrectResponses: List[UserResponse]):
      UserResponses =
    UserResponses(correctResponsesInARow, incorrectResponses)

  // Example: nachlösen:1,7,9;6
  def toCustomFormat(strBuilder: StringBuilder, mainSeparator: String): StringBuilder = {
    if (!correctResponsesInARow.isEmpty)
      StringUtil.mkString(strBuilder, correctResponsesInARow, responsePromptNumber, ',')
    if (!incorrectResponses.isEmpty) {
      strBuilder.append(';')
      StringUtil.mkString(strBuilder, incorrectResponses, responsePromptNumber, ',')
    }
    strBuilder
  }

  def responsePromptNumber(strBuilder: StringBuilder, response: UserResponse) =
    strBuilder.append(response.promptNumber)

}

object UserResponses {

  val userResponsesCorrectResponsesLens = Lens.lensu(
      get = (_: UserResponses).correctResponsesInARow,
      set = (ur: UserResponses, urs: List[UserResponse]) => ur.copy(correctResponsesInARow = urs))

  val userResponsesIncorrectResponsesLens = Lens.lensu(
      get = (_: UserResponses).incorrectResponses,
      set = (ur: UserResponses, urs: List[UserResponse]) => ur.copy(incorrectResponses = urs))
}


/*
 * Criteria used to check if a quiz item is "presentable".
 *
 * numCorrectResponsesInARowDesired: how many times this item should have been answered correctly
 * diffInPromptNumMinimum: how long ago it was last answered - may be None to omit this criterion
 */
case class Criteria(numCorrectResponsesInARowDesired: Int, diffInPromptNumMinimum: Int) {
  def isPresentable(currentPromptNum : Int, promptNumInMostRecentResponse: Option[Int],
      numCorrectResponsesInARow: Int): Boolean = {
    def wasNotTooRecentlyUsed = promptNumInMostRecentResponse.forall {
      case promptNumInMostRecentResponse =>
          val diffInPromptNum = currentPromptNum - promptNumInMostRecentResponse
          diffInPromptNum >= diffInPromptNumMinimum
    }
    (numCorrectResponsesInARow == numCorrectResponsesInARowDesired) && wasNotTooRecentlyUsed
  }
}

object Criteria {
  /*
   * Criteria sets that determine whether the current item is presentable or not.
   */
  val criteriaSets = Seq[Criteria](
    Criteria(numCorrectResponsesInARowDesired = 1, diffInPromptNumMinimum = 5),
    Criteria(numCorrectResponsesInARowDesired = 2, diffInPromptNumMinimum = 15),
    Criteria(numCorrectResponsesInARowDesired = 3, diffInPromptNumMinimum = 25),
    Criteria(numCorrectResponsesInARowDesired = 4, diffInPromptNumMinimum = 100)
  )

  def maxDiffInPromptNumMinimum = criteriaSets.map(_.diffInPromptNumMinimum).max
}