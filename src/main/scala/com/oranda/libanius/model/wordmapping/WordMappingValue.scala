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

package com.oranda.libanius.model.wordmapping

import com.oranda.libanius.model.UserResponse
import com.oranda.libanius.util.StringUtil
import com.oranda.libanius.dependencies.AppDependencyAccess
import com.oranda.libanius.model.quizitem.{TextValue, QuizItem, Value}

import java.lang.StringBuilder

case class WordMappingValue(override val value: String,
    correctAnswersInARow: List[UserResponse] = Nil,
    incorrectAnswers: List[UserResponse] = Nil)
  extends Value[String](value) {

  def updated(correctAnswersInARow: List[UserResponse], incorrectAnswers: List[UserResponse]):
      WordMappingValue =
    WordMappingValue(value, correctAnswersInARow, incorrectAnswers)

  override def matches(otherText: String) = value == otherText

  override def hasSameStart(otherValue: String): Int => Boolean =
    TextValue.hasSameStart(value, otherValue)

  override def hasSameEnd(otherValue: String): Int => Boolean =
    TextValue.hasSameEnd(value, otherValue)

  def userAnswers = correctAnswersInARow ++ incorrectAnswers

  def numCorrectAnswersInARow = correctAnswersInARow.size

  // Example: nachlösen:1,7,9;6
  def toCustomFormat(strBuilder: StringBuilder, mainSeparator: String): StringBuilder = {
    strBuilder.append(value)

    if (!correctAnswersInARow.isEmpty || !incorrectAnswers.isEmpty)
      strBuilder.append(mainSeparator)
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

  def addUserAnswersBatch(correctPromptNumStrs: List[String],
      incorrectPromptNumStrs: List[String]): WordMappingValue = {
    val newCorrectAnswersInARow = correctPromptNumStrs.map(correctPromptNum =>
        new UserResponse(correctPromptNum.toInt))
    val newIncorrectAnswers = incorrectPromptNumStrs.map(incorrectPromptNum =>
        new UserResponse(incorrectPromptNum.toInt))
    updated(newCorrectAnswersInARow, newIncorrectAnswers)
  }
}

object WordMappingValue extends AppDependencyAccess {

  def apply(quizItem: QuizItem): WordMappingValue =
    WordMappingValue(quizItem.correctResponse.toString,
        quizItem.userResponses.correctResponsesInARow,
        quizItem.userResponses.incorrectResponses)

  // Example: text = "nachlösen|1,7,9;6"
  def fromCustomFormat(str: String, mainSeparator: String): WordMappingValue = {

    import com.oranda.libanius.util.StringUtil.RichString
    str.optionalIndex(mainSeparator) match {
      case Some(index) =>
        val strResponse = str.substring(0, index)
        val strAllAnswers = str.substring(index + mainSeparator.length)

        val wmv = WordMappingValue(strResponse.trim)
        if (strAllAnswers.isEmpty)
          wmv
        else {
          val (correctAnswers, incorrectAnswers) = parseAnswers(strAllAnswers)
          wmv.addUserAnswersBatch(correctAnswers, incorrectAnswers)
        }
      case None => WordMappingValue(str.trim)
    }
  }

  private def parseAnswers(strAllAnswers: String): (List[String], List[String]) = {
    // This code needs to be both fast and thread-safe so special "splitters" are used.
    val allAnswersSplitter = stringSplitterFactory.getSplitter(';')
    val answersSplitter = stringSplitterFactory.getSplitter(',')
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
    (correctAnswers, incorrectAnswers)
  }
}