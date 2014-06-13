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

import scala.util.Random
import com.oranda.libanius.model.UserResponses
import com.oranda.libanius.model.quizgroup.QuizGroupHeader
import com.oranda.libanius.dependencies.AppDependencyAccess

/**
 * Quiz item data holder:
 * contains whatever information is necessary for the view, and for updating the backing data.
 */
case class QuizItemViewWithChoices(
    val quizItem: QuizItem,
    val qgCurrentPromptNumber: Int,
    val quizGroupHeader: QuizGroupHeader,
    val falseAnswers: List[String],
    val numCorrectResponsesInARow: Int,
    val numCorrectResponsesRequired: Int,
    val useMultipleChoice: Boolean) {

  lazy val prompt = quizItem.prompt
  lazy val correctResponse = quizItem.correctResponse
  lazy val promptType = quizGroupHeader.promptType
  lazy val responseType = quizGroupHeader.responseType

  lazy val allChoices: List[String] = choicesInRandomOrder(quizItem.userResponses, falseAnswers)

  def isComplete = numCorrectResponsesInARow >= numCorrectResponsesRequired

  def choicesInRandomOrder(quizValue: UserResponses, otherChoices: List[String]): List[String] = {
    val allChoices = quizItem.correctResponse.value :: otherChoices
    Random.shuffle(allChoices)
  }
}
