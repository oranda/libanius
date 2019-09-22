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

package com.oranda.libanius.model

import java.lang.StringBuilder

import scalaz._

case class UserResponsesAll(correctResponsesInARow: List[UserResponse] = Nil,
    incorrectResponses: List[UserResponse] = Nil) extends ModelComponent {

  def userResponses = correctResponsesInARow ++ incorrectResponses

  def add(userResponse: UserResponse, wasCorrect: Boolean): UserResponsesAll =
    if (wasCorrect)
      UserResponsesAll.userResponsesCorrectResponsesLens.set(this,
          userResponse :: correctResponsesInARow)
    else {
      // on an incorrect answer, old correct answers are discarded
      val ur = UserResponsesAll.userResponsesCorrectResponsesLens.set(this, Nil)
      val newIncorrectResponses = userResponse :: incorrectResponses
      UserResponsesAll.userResponsesIncorrectResponsesLens.set(ur, newIncorrectResponses)
    }

  /*
   * See if this quiz item meets any of the defined criteria sets that would make it presentable.
   * (Intended to be called over many quiz items until one fits.)
   */
  protected[model] def isPresentable(currentPromptNum: Int, repetitionInterval: Int): Boolean =
    wasNotTooRecentlyUsed(currentPromptNum, promptNumInMostRecentResponse, repetitionInterval)

  def wasNotTooRecentlyUsed(
      currentPromptNum: Int,
      promptNumInMostRecentResponse: Option[Int],
      repetitionInterval: Int): Boolean =
    promptNumInMostRecentResponse.forall {
      case promptNumInMostRecentResponse =>
        val diffInPromptNum = currentPromptNum - promptNumInMostRecentResponse
        diffInPromptNum >= repetitionInterval
    }

  def numCorrectResponsesInARow = correctResponsesInARow.length

  def promptNumInMostRecentResponse: Option[Int] =
    correctResponsesInARow.headOption.orElse(incorrectResponses.headOption).orElse(None).
        map(_.promptNumber)

  def updated(correctResponsesInARow: List[UserResponse], incorrectResponses: List[UserResponse]):
      UserResponsesAll =
    UserResponsesAll(correctResponsesInARow, incorrectResponses)

  def responsePromptNumber(strBuilder: StringBuilder, response: UserResponse) =
    strBuilder.append(response.promptNumber)

}

object UserResponsesAll {

  val userResponsesCorrectResponsesLens = Lens.lensu(
    get = (_: UserResponsesAll).correctResponsesInARow,
    set = (ur: UserResponsesAll, urs: List[UserResponse]) => ur.copy(correctResponsesInARow = urs))

  val userResponsesIncorrectResponsesLens = Lens.lensu(
    get = (_: UserResponsesAll).incorrectResponses,
    set = (ur: UserResponsesAll, urs: List[UserResponse]) => ur.copy(incorrectResponses = urs))
}
