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

import com.oranda.libanius.model.UserResponses

/*
 * A connection between two things, and user information associated with the connection.
 *
 * prompt: the cue given to the user
 * response: the desired response
 * userResponses: a history of actual responses
 *
 * Examples of QuizItems:
 *  - a question and an answer in the quiz
 *  - a word and a translation
 */
case class QuizItem(prompt: TextValue, response: TextValue,
    userResponses: UserResponses = UserResponses()) {

  def samePromptAndResponse(other: QuizItem) = other.prompt == prompt && other.response == response
  def isPresentable(currentPromptNumber: Int) = userResponses.isPresentable(currentPromptNumber)

  def promptNumInMostRecentAnswer = userResponses.promptNumInMostRecentAnswer
  def numCorrectAnswersInARow = userResponses.numCorrectAnswersInARow

}

object QuizItem {
  def apply(prompt: String, response: String): QuizItem =
    QuizItem(TextValue(prompt), TextValue(response), new UserResponses)
}