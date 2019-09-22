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

package com.oranda.libanius.model.action

import org.specs2.mutable.Specification

import com.oranda.libanius.dependencies.AppDependencyAccess
import com.oranda.libanius.model.TestData._
import com.oranda.libanius.model.quizitem.{TextValueOps, QuizItem}
import com.oranda.libanius.model.quizgroup.{QuizGroup, QuizGroupWithHeader, QuizGroupMemoryLevel}
import com.oranda.libanius.model.{UserResponsesAll, UserResponse}


import QuizItemSource._
import modelComponentsAsQuizItemSources._
import QuizItemSourceSpec._


class QuizItemSourceSpec extends Specification with AppDependencyAccess {
  "a QuizItemSource " should {

    "find a presentable quiz item" in {
      val quizItemViewWithChoices = produceQuizItem(quiz, NoParams()).get
      quizItemViewWithChoices.prompt mustEqual TextValueOps.TextValue("against")
    }

    "present an item from a memory level that has been answered before after five prompts" in {
      var qgmlLocal = makeQgMemLevel
      val quizItem0 = produceQuizItem(qgmlLocal, CurrentPromptNumber(0)).get
      quizItem0.prompt.value mustEqual "against"
      qgmlLocal = QuizItemSourceSpec.updateWithUserAnswer(qgmlLocal, quizItem0, 0)

      for (promptNum <- 1 until 5)
        qgmlLocal = QuizItemSourceSpec.pullQuizItemAndAnswerCorrectly(qgmlLocal, promptNum)

      val quizItem5: Option[QuizItem] = produceQuizItem(qgmlLocal, CurrentPromptNumber(5))
      quizItem5.get.prompt.value mustEqual "against"
    }

    "find a presentable quiz item from a quiz group" in {
      pullQuizItem(qgWithHeader).promptAndResponse mustEqual ("winner", "Siegerin")
    }

    /*
     * This assumes the following Criteria in UserResponses:
     * Criteria(numCorrectResponsesInARowDesired = 1, diffInPromptNumMinimum = 5),
     */
    "present an item that has been answered before after five prompts" in {
      var qgwhLocal = makeSimpleQgWithHeader
      val quizItem0 = produceQuizItem(qgwhLocal.quizGroup, NoParams()).get
      quizItem0.prompt.value mustEqual "en route" // "against"
      qgwhLocal = QuizGroupWithHeader(qgwhLocal.header,
          updatedWithUserResponse(qgwhLocal, quizItem0))
      for (promptNum <- 1 until 5)
        qgwhLocal = pullQuizItemAndAnswerCorrectly(qgwhLocal)

      val quizItem5 = produceQuizItem(qgwhLocal.quizGroup, NoParams())
      quizItem5.get.prompt.value mustEqual "en route" // "against"
    }

    "find a presentable quiz item from a memory level" in {
      pullQuizItem(qgMemLevel, 0).promptAndResponse mustEqual ("against", "wider")
    }
  }

}

object QuizItemSourceSpec {

  case class QuizGroupWithQuizItem(quizGroup: QuizGroup, prompt: String, response: String) {
    def promptAndResponse = (prompt, response)
  }

  case class QgmlWithQuizItem(qgml: QuizGroupMemoryLevel, prompt: String, response: String) {
    def promptAndResponse = (prompt, response)
  }

  // pullQuizItem for QuizGroupWithHeader
  def pullQuizItem(qgwh: QuizGroupWithHeader): QuizGroupWithQuizItem =
    pullQuizItem(qgwh.quizGroup)

  // pullQuizItem for QuizGroup
  def pullQuizItem(qg: QuizGroup): QuizGroupWithQuizItem = {
    val quizItemOpt = produceQuizItem(qg, NoParams())
    assert(quizItemOpt.isDefined)
    val quizItem = quizItemOpt.get
    // Each time a quiz item is pulled, a user answer must be set
    val qgUpdated = updatedWithUserResponse(qg, quizItem)
    QuizGroupWithQuizItem(qgUpdated, quizItem.prompt.value, quizItem.correctResponse.value)
  }

  // pullQuizItem for QuizGroupMemoryLevel
  def pullQuizItem(qgml: QuizGroupMemoryLevel, currentPromptNumber: Int): QgmlWithQuizItem = {

    val quizItem: Option[QuizItem] =
      produceQuizItem(qgml, CurrentPromptNumber(currentPromptNumber))

    assert(quizItem.isDefined)
    // Each time a quiz item is pulled, a user answer must be set
    val qgmlUpdated1 = qgml.updatedWithUserAnswer(quizItem.get.prompt,
      quizItem.get.correctResponse, wasCorrect = true, UserResponsesAll(), new UserResponse(0))
    val qgmlUpdated2 = qgmlUpdated1 - quizItem.get
    QgmlWithQuizItem(qgmlUpdated2, quizItem.get.prompt.value, quizItem.get.correctResponse.value)
  }

  def pullQuizItemAndAnswerCorrectly(
      qgml: QuizGroupMemoryLevel,
      currentPromptNumber: Int): QuizGroupMemoryLevel = {
    val quizItem = produceQuizItem(qgml, CurrentPromptNumber(currentPromptNumber)).get
    updateWithUserAnswer(qgml, quizItem, currentPromptNumber)
  }

  def pullQuizItemAndAnswerCorrectly(qgwh: QuizGroupWithHeader): QuizGroupWithHeader = {
    val quizItem = produceQuizItem(qgwh.quizGroup, NoParams()).get
    QuizGroupWithHeader(qgwh.header, updatedWithUserResponse(qgwh.quizGroup, quizItem))
  }


  // updatedWithUserResponse for QuizGroup
  def updatedWithUserResponse(qg: QuizGroup, quizItem: QuizItem): QuizGroup = {
    val userResponse = new UserResponse(qg.currentPromptNumber)
    val wasCorrect = true
    val quizItemUpdated = quizItem.updatedWithUserResponse(
      quizItem.correctResponse, wasCorrect, userResponse)
    val prevMemLevel = quizItemUpdated.numCorrectResponsesInARow
    qg.updateWithQuizItem(quizItemUpdated, wasCorrect, prevMemLevel)
  }

  // updatedWithUserResponse for QuizGroupMemoryLevel
  def updateWithUserAnswer(
      qgml: QuizGroupMemoryLevel,
      quizItem: QuizItem,
      currentPromptNumber: Int): QuizGroupMemoryLevel = {
    val userAnswer = new UserResponse(currentPromptNumber)
    qgml.updatedWithUserAnswer(quizItem.prompt, quizItem.correctResponse, wasCorrect = true,
      UserResponsesAll(), userAnswer)
  }
}
