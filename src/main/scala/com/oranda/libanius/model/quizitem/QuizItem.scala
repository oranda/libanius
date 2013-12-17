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

import com.oranda.libanius.model.{UserResponse, UserResponses}
import com.oranda.libanius.model.wordmapping.WordMappingValue

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
    userResponses: UserResponses = new UserResponses()) {

  def samePromptAndResponse(other: QuizItem) =
    other.prompt == prompt && other.correctResponse == correctResponse
  def isPresentable(currentPromptNumber: Int) = userResponses.isPresentable(currentPromptNumber)

  def promptNumInMostRecentAnswer = userResponses.promptNumInMostRecentResponse
  def numCorrectAnswersInARow = userResponses.numCorrectResponsesInARow

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

  def fromCustomFormat(strPromptResponse: String, mainSeparator: String = "|") = {
    val i = strPromptResponse.indexOf(mainSeparator)
    val strPrompt = strPromptResponse.substring(0, i).trim
    val strResponseAndUserInfo = strPromptResponse.substring(i + mainSeparator.length)

    val wmv = WordMappingValue.fromCustomFormat(strResponseAndUserInfo, mainSeparator)
    val userResponses = UserResponses(wmv.correctAnswersInARow, wmv.incorrectAnswers)
    QuizItem(TextValue(strPrompt), TextValue(wmv.value), userResponses)
  }
}