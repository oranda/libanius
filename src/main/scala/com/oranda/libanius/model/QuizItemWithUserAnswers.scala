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
import QuizItemWithUserAnswers._
import com.oranda.libanius.io.PlatformIO
import com.oranda.libanius.dependencies.{AppDependencies, Conf}

abstract class QuizItemWithUserAnswers[T](correctAnswersInARow: List[UserAnswer],
    incorrectAnswers: List[UserAnswer]) extends ModelComponent {

  def self: T

  def updated(correctAnswersInARow: List[UserAnswer], incorrectAnswers: List[UserAnswer]): T

  def userAnswers = correctAnswersInARow ++ incorrectAnswers

  def addUserAnswer(userAnswer : UserAnswer): T =
    if (userAnswer.wasCorrect)
      updated(userAnswer :: correctAnswersInARow, incorrectAnswers)
    else
      updated(Nil, userAnswer :: incorrectAnswers) // old correct answers are discarded

  
  def addUserAnswersBatch(correctPromptNumStrs: List[String],
      incorrectPromptNumStrs: List[String]): T = {
    // TODO: see if an imperative version is faster
    val newCorrectAnswersInARow = correctPromptNumStrs.map(correctPromptNum =>
        new UserAnswer(wasCorrect = false, promptNumber = correctPromptNum.toInt))
    val newIncorrectAnswers = incorrectPromptNumStrs.map(incorrectPromptNum =>
        new UserAnswer(wasCorrect = false, promptNumber = incorrectPromptNum.toInt))
    updated(newCorrectAnswersInARow, newIncorrectAnswers)
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


  def isUnfinished: Boolean = numCorrectAnswersInARow <
      AppDependencies.conf.numCorrectAnswersRequired
  
  def numCorrectAnswersInARow = correctAnswersInARow.length
  
  def promptNumInMostRecentAnswer: Option[Int] =
    correctAnswersInARow.headOption.orElse(incorrectAnswers.headOption).orElse(None).
        map(_.promptNumber)
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

object QuizItemWithUserAnswers {
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
