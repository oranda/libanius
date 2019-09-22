/*
 * Libanius
 * Copyright (C) 2012-2019 James McCabe <james@oranda.com>
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

import com.oranda.libanius.model.quizitem.TextValueOps.TextValue

import scala.language.implicitConversions
import com.oranda.libanius.model.{UserResponsesAll, ModelComponent, UserResponse}

sealed trait QuizItem extends ModelComponent {
  def prompt: TextValue
  def correctResponse: TextValue
  def userResponses: UserResponsesAll

  def promptNumInMostRecentAnswer: Option[Int]
  def numCorrectResponsesInARow: Int

  def samePromptAndResponse(other: QuizItem): Boolean

  def isPresentable(currentPromptNumber: Int, repetitionInterval: Int): Boolean

  def looselyMatches(userResponse: String): Boolean

  def isValid: Boolean

  def updatedWithUserResponse(
      response: TextValue,
      wasCorrect: Boolean,
      userResponse: UserResponse): QuizItem
}

/*
 * A connection between two things, and user information associated with the connection.
 *
 * prompt: the cue given to the user
 * correctResponse: the desired correctResponse
 * userResponses: a history of actual responses
 *
 * TODO: split this into two or more subtypes, representing:
 *  1. a QuizItem for a word and a translation
 *  2. a QuizItem for a question and an answer
 */
case class QuizItemConcrete(
    prompt: TextValue,
    correctResponse: TextValue,
    userResponses: UserResponsesAll = new UserResponsesAll())
  extends QuizItem {

  def promptNumInMostRecentAnswer = userResponses.promptNumInMostRecentResponse
  def numCorrectResponsesInARow = userResponses.numCorrectResponsesInARow

  def samePromptAndResponse(other: QuizItem) =
    other.prompt == prompt && other.correctResponse == correctResponse

  def isPresentable(currentPromptNumber: Int, repetitionInterval: Int) =
    userResponses.isPresentable(currentPromptNumber, repetitionInterval)

  def looselyMatches(userResponse: String): Boolean =
    correctResponse.looselyMatches(userResponse)

  def isValid: Boolean =
    !prompt.isEmpty && !correctResponse.isEmpty &&
      !(prompt.toLowerCase == correctResponse.toLowerCase)

  def updatedWithUserResponse(
      response: TextValue,
      wasCorrect: Boolean,
      userResponse: UserResponse): QuizItem = {
    val userResponsesUpdated = userResponses.add(userResponse, wasCorrect)
    QuizItemConcrete(prompt, response, userResponsesUpdated)
  }
}

/**
 * Instantiate quiz items using the default QuizItemConcrete.
 */
object QuizItem {

  def apply(
      prompt: TextValue,
      response: TextValue,
      userResponses: UserResponsesAll = new UserResponsesAll()): QuizItem =
    QuizItemConcrete(prompt, response, userResponses)

  def apply(prompt: String, response: String): QuizItem =
    QuizItemConcrete(prompt, response)

  def apply(
      prompt: String,
      response: String,
      correctResponses: List[Int],
      incorrectResponses: List[Int]): QuizItem =
    QuizItemConcrete(prompt, response,
        new UserResponsesAll(correctResponses.map(UserResponse),
        incorrectResponses.map(UserResponse)))
}
