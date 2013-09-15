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

package com.oranda.libanius.model

import com.oranda.libanius.util.StringUtil
import UserResponses._

import java.lang.StringBuilder

import scalaz._

case class UserResponses(correctAnswersInARow: List[UserResponse] = Nil,
    incorrectAnswers: List[UserResponse] = Nil)
  extends ModelComponent {

  def updated(value: String, correctAnswersInARow: List[UserResponse],
      incorrectAnswers: List[UserResponse]): UserResponses =
    new UserResponses(correctAnswersInARow, incorrectAnswers)

  def userAnswers = correctAnswersInARow ++ incorrectAnswers

  def addUserAnswer(userAnswer : UserResponse, wasCorrect: Boolean): UserResponses =
    if (wasCorrect)
      UserResponses.userResponsesCorrectAnswersLens.set(this, userAnswer :: correctAnswersInARow)
    else {
      // on an incorrect answer, old correct answers are discarded
      val ur = UserResponses.userResponsesCorrectAnswersLens.set(this, Nil)
      UserResponses.userResponsesIncorrectAnswersLens.set(ur, userAnswer :: incorrectAnswers)
    }


  /*
   * See if this quiz item meets any of the defined criteria sets that would make it presentable.
   * (Intended to be called over many quiz items until one fits.)
   */
  def isPresentable(currentPromptNum : Int): Boolean =
    isPresentable(currentPromptNum, promptNumInMostRecentAnswer, numCorrectAnswersInARow)

  protected[model] def isPresentable(currentPromptNum : Int,
      promptNumInMostRecentAnswer: Option[Int], numCorrectAnswersInARow: Int): Boolean =
    numCorrectAnswersInARow == 0 || criteriaSets.exists(
        _.isPresentable(currentPromptNum, promptNumInMostRecentAnswer, numCorrectAnswersInARow))


  def isUnfinished: Boolean = numCorrectAnswersInARow < conf.numCorrectAnswersRequired
  
  def numCorrectAnswersInARow = correctAnswersInARow.length
  
  def promptNumInMostRecentAnswer: Option[Int] =
    correctAnswersInARow.headOption.orElse(incorrectAnswers.headOption).orElse(None).
        map(_.promptNumber)


  def updated(correctAnswersInARow: List[UserResponse], incorrectAnswers: List[UserResponse]):
      UserResponses =
    UserResponses(correctAnswersInARow, incorrectAnswers)

  // Example: nachlösen:1,7,9;6
  def toCustomFormat(strBuilder: StringBuilder): StringBuilder = {
    if (!correctAnswersInARow.isEmpty || !incorrectAnswers.isEmpty)
      strBuilder.append(':')
    if (!correctAnswersInARow.isEmpty)
      StringUtil.mkString(strBuilder, correctAnswersInARow, answerPromptNumber, ',')
    if (!incorrectAnswers.isEmpty) {
      strBuilder.append(';')
      StringUtil.mkString(strBuilder, incorrectAnswers, answerPromptNumber, ',')
    }
    strBuilder
  }

  def answerPromptNumber(strBuilder: StringBuilder, answer: UserResponse) =
    strBuilder.append(answer.promptNumber)

}

object UserResponses {

  val userResponsesCorrectAnswersLens = Lens.lensu(
      get = (_: UserResponses).correctAnswersInARow,
      set = (ur: UserResponses, urs: List[UserResponse]) => ur.copy(correctAnswersInARow = urs))

  val userResponsesIncorrectAnswersLens = Lens.lensu(
      get = (_: UserResponses).incorrectAnswers,
      set = (ur: UserResponses, urs: List[UserResponse]) => ur.copy(incorrectAnswers = urs))

  /*
   * Criteria sets that determine whether the current item is presentable or not.
   */
  val criteriaSets = Seq[Criteria](
    Criteria(numCorrectAnswersInARowDesired = 1, diffInPromptNumMinimum = 5),
    Criteria(numCorrectAnswersInARowDesired = 2, diffInPromptNumMinimum = 40),
    Criteria(numCorrectAnswersInARowDesired = 3, diffInPromptNumMinimum = 800)
    /*(4, 5000),*/
  )
}

/*
 * Criteria used to check if a quiz item is "presentable".
 *
 * numCorrectAnswersInARowDesired: how many times this item should have been answered correctly
 * diffInPromptNumMinimum: how long ago it was last answered - may be None to omit this criterion
 */
case class Criteria(numCorrectAnswersInARowDesired: Int, diffInPromptNumMinimum: Int) {
  def isPresentable(currentPromptNum : Int, promptNumInMostRecentAnswer: Option[Int],
      numCorrectAnswersInARow: Int): Boolean = {
    def wasNotTooRecentlyUsed = promptNumInMostRecentAnswer.forall {
      case promptNumInMostRecentAnswer =>
          val diffInPromptNum = currentPromptNum - promptNumInMostRecentAnswer
          diffInPromptNum >= diffInPromptNumMinimum
    }

    (numCorrectAnswersInARow == numCorrectAnswersInARowDesired) && wasNotTooRecentlyUsed
  }
}
