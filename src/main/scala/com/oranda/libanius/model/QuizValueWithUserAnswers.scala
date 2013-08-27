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

import com.oranda.libanius.dependencies.AppDependencies
import com.oranda.libanius.util.StringUtil
import QuizValueWithUserAnswers._

case class QuizValueWithUserAnswers(value: String,
    correctAnswersInARow: List[UserAnswer] = Nil, incorrectAnswers: List[UserAnswer] = Nil)
  extends ModelComponent {

  def updated(value: String, correctAnswersInARow: List[UserAnswer],
      incorrectAnswers: List[UserAnswer]): QuizValueWithUserAnswers =
    new QuizValueWithUserAnswers(value, correctAnswersInARow, incorrectAnswers)

  def userAnswers = correctAnswersInARow ++ incorrectAnswers

  def addUserAnswer(userAnswer : UserAnswer): QuizValueWithUserAnswers =
    if (userAnswer.wasCorrect)
      updated(value, userAnswer :: correctAnswersInARow, incorrectAnswers)
    else
      updated(value, Nil, userAnswer :: incorrectAnswers) // old correct answers are discarded

  
  def addUserAnswersBatch(correctPromptNumStrs: List[String],
      incorrectPromptNumStrs: List[String]): QuizValueWithUserAnswers = {
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


  def updated(correctAnswersInARow: List[UserAnswer], incorrectAnswers: List[UserAnswer]):
      QuizValueWithUserAnswers =
    QuizValueWithUserAnswers(value, correctAnswersInARow, incorrectAnswers)

  override def toString = value   // e.g. "unterrichten"

  // Example: nachlösen:1,7,9;6
  def toCustomFormat(strBuilder: StringBuilder): StringBuilder = {
    strBuilder.append(value)

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

  def answerPromptNumber(strBuilder: StringBuilder, answer: UserAnswer) =
    strBuilder.append(answer.promptNumber)


  def hasSameStart(otherValue: String) =
    (numOfLetters: Int) => otherValue != value &&
      value.take(numOfLetters) == otherValue.take(numOfLetters)

  def hasSameEnd(otherValue: String) =
    (numOfLetters: Int) => otherValue != value &&
      value.takeRight(numOfLetters) == otherValue.takeRight(numOfLetters)
}

object QuizValueWithUserAnswers {

  /*
   * Criteria sets that determine whether the current item is presentable or not.
   */
  val criteriaSets = Seq[Criteria](
    Criteria(numCorrectAnswersInARowDesired = 1, diffInPromptNumMinimum = 5),
    Criteria(numCorrectAnswersInARowDesired = 2, diffInPromptNumMinimum = 40),
    Criteria(numCorrectAnswersInARowDesired = 3, diffInPromptNumMinimum = 800)
    /*(4, 5000),*/
  )

  // Example: str = "nachlösen:1,7,9;6"
  def fromCustomFormat(str: String): QuizValueWithUserAnswers = {

    // This code needs to be both fast and thread-safe.
    val wmvSplitter = AppDependencies.stringSplitterFactory.getSplitter(':')
    val allAnswersSplitter = AppDependencies.stringSplitterFactory.getSplitter(';')
    val answersSplitter = AppDependencies.stringSplitterFactory.getSplitter(',')

    wmvSplitter.setString(str)
    var wmv = new QuizValueWithUserAnswers(wmvSplitter.next, Nil, Nil)
    if (wmvSplitter.hasNext) {
      val strAllAnswers = wmvSplitter.next
      allAnswersSplitter.setString(strAllAnswers)

      val correctPromptNums = allAnswersSplitter.next
      answersSplitter.setString(correctPromptNums)
      val correctAnswers = answersSplitter.toList
      val incorrectAnswers =
        if (allAnswersSplitter.hasNext) {
          val incorrectPromptNums = allAnswersSplitter.next
          answersSplitter.setString(incorrectPromptNums)
          answersSplitter.toList
        } else
          Nil
      wmv = wmv.addUserAnswersBatch(correctAnswers, incorrectAnswers)
    }
    wmv
  }
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
