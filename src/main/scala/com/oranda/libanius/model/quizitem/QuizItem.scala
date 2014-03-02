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

package com.oranda.libanius.model.quizitem

import com.oranda.libanius.model.{Criteria, UserResponse, UserResponses}
import com.oranda.libanius.model.wordmapping.WordMappingValue
import com.oranda.libanius.dependencies.AppDependencyAccess

/*
 * A connection between two things, and user information associated with the connection.
 *
 * prompt: the cue given to the user
 * correctResponse: the desired correctResponse
 * userResponses: a history of actual responses
 *
 * Examples of QuizItems:
 *  - a question and an answer in the quiz
 *  - a word and a translation
 */
case class QuizItem(prompt: TextValue, correctResponse: TextValue,
    userResponses: UserResponses = new UserResponses()) extends AppDependencyAccess {

  def promptNumInMostRecentAnswer = userResponses.promptNumInMostRecentResponse
  def numCorrectAnswersInARow = userResponses.numCorrectResponsesInARow

  def isComplete = numCorrectAnswersInARow >= Criteria.numCorrectResponsesRequired

  def samePromptAndResponse(other: QuizItem) =
    other.prompt == prompt && other.correctResponse == correctResponse
  def isPresentable(currentPromptNumber: Int) =
    userResponses.isPresentable(currentPromptNumber)

  def looselyMatches(userResponse: String): Boolean =
    correctResponse.looselyMatches(userResponse)

  def toCustomFormat(strBuilder: java.lang.StringBuilder, mainSeparator: String) = {
    strBuilder.append(prompt).append(mainSeparator).append(correctResponse).
        append(mainSeparator)
    userResponses.toCustomFormat(strBuilder, mainSeparator)
    strBuilder
  }

}

object QuizItem {
  def apply(prompt: String, response: String): QuizItem =
    QuizItem(TextValue(prompt), TextValue(response))

  def apply(prompt: String, response: String, correctResponses: List[Int],
      incorrectResponses: List[Int]): QuizItem =
    QuizItem(TextValue(prompt), TextValue(response),
        new UserResponses(correctResponses.map(UserResponse(_)),
        incorrectResponses.map(UserResponse(_))))

  def fromCustomFormat(strPromptResponse: String, mainSeparator: String = "|"): QuizItem = {
    val i = strPromptResponse.indexOf(mainSeparator)
    val strPrompt = strPromptResponse.substring(0, i).trim
    val strResponseAndUserInfo = strPromptResponse.substring(i + mainSeparator.length)

    val wmv = WordMappingValue.fromCustomFormat(strResponseAndUserInfo, mainSeparator)
    val userResponses = UserResponses(wmv.correctAnswersInARow, wmv.incorrectAnswers)
    QuizItem(TextValue(strPrompt), TextValue(wmv.value), userResponses)
  }
}