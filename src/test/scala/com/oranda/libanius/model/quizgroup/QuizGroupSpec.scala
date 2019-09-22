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

package com.oranda.libanius.model.quizgroup

import org.specs2.mutable.Specification
import com.oranda.libanius.dependencies.AppDependencyAccess
import com.oranda.libanius.model.quizitem.QuizItem

import com.oranda.libanius.model.TestData._

import com.oranda.libanius.model.action._
import QuizItemSource._
import modelComponentsAsQuizItemSources._
import com.oranda.libanius.model.action.QuizItemSourceSpec._

import com.oranda.libanius.model.action.wrongchoices._

class QuizGroupSpec extends Specification with AppDependencyAccess {

  "a quiz group" should {

    "find values for a prompt" in {
      qgWithHeader.findResponsesFor("on") mustEqual List("auf")
    }

    "accept an updated prompt number" in {
      val qgUpdated = quizGroup.updatedPromptNumber
      qgUpdated.currentPromptNumber mustEqual 11
    }

    "accept the addition of a new word-mapping" in {
      quizGroup.contains("good") mustEqual false
      val qgUpdated = quizGroup + QuizItem("good", "gut")
      qgUpdated.contains("good") mustEqual true
    }

    "update memory levels on a user response" in {
      qgwhSimple.quizGroup.levels(1).size mustEqual 0
      val quizItem = produceQuizItem(qgwhSimple.quizGroup, NoParams())
      val qgUpdated = updatedWithUserResponse(qgwhSimple.quizGroup, quizItem.get)
      qgUpdated.levels(1).size mustEqual 1
    }

    "accept new values for an existing word-mapping" in {
      val valuesForAgainst = quizGroup.findResponsesFor("against")
      valuesForAgainst.size mustEqual 1
      val qgUpdated = quizGroup + QuizItem("against", "gegen")
      qgUpdated.findResponsesFor("against").size mustEqual 2
    }

    "remove a quiz pair" in {
      val itemToRemove = QuizItem("against", "wider")
      val qgUpdated = quizGroup - itemToRemove
      qgUpdated.contains("against") mustEqual false
    }

    "add a new quiz item to the front of its queue" in {
      val qgUpdated = quizGroupSimple + QuizItem("to exchange", "tauschen")
      val foundQuizItem = QuizItemSource.produceQuizItem(qgUpdated, NoParams())
      foundQuizItem mustEqual Some(QuizItem("to exchange", "tauschen"))
    }

    "move an existing quiz pair to the front of its queue" in {
      val numPromptsBefore = quizGroup.numPrompts
      val qgUpdated = quizGroup + QuizItem("sweeps", "streicht")
      val numPromptsAfter = qgUpdated.numPrompts
      numPromptsAfter mustEqual numPromptsBefore
      val foundQuizItem = QuizItemSource.produceQuizItem(
        qgUpdated.levels.head,
          CurrentPromptNumber(qgUpdated.currentPromptNumber))
      foundQuizItem mustEqual Some(QuizItem("sweeps", "streicht"))
    }

    "add a quiz pair where only the prompt already exists" in {
      val sizeBefore = quizGroup.size
      val qgUpdated = quizGroup + QuizItem("on", "zu")
      val sizeAfter = qgUpdated.size
      sizeAfter mustEqual sizeBefore + 1
      val foundQuizItem = QuizItemSource.produceQuizItem(
        qgUpdated.levels.head, CurrentPromptNumber(qgUpdated.currentPromptNumber))
      foundQuizItem mustEqual Some(QuizItem("on", "zu"))
    }

    "add more than one new quiz pair" in {
      val qgSimple = makeSimpleQuizGroup
      val qgUpdated1 = qgSimple + QuizItem("to exchange", "tauschen")
      val qgUpdated2 = qgUpdated1 + QuizItem("whole", "ganz")
      val QuizGroupWithQuizItem(qg, prompt, response) = pullQuizItem(qgUpdated2)
      (prompt, response) mustEqual ("whole", "ganz")
      pullQuizItem(qg).promptAndResponse mustEqual ("to exchange", "tauschen")
    }

    val memLevel2 = QuizGroupMemoryLevel(correctResponsesInARow = 2,
        repetitionInterval = 15, totalResponses = 2, numCorrectResponses = 1)
    val qg = QuizGroup(Map(2 -> memLevel2))

    "be updated for a correct answer" in {
      val updatedQg = qg.incrementResponsesCorrect(2)
      updatedQg.totalResponses(2) mustEqual 3
      updatedQg.numCorrectResponses(2) mustEqual 2
    }

    "be updated for a incorrect answer" in {
      val updatedQg = qg.incrementResponsesIncorrect(2)
      updatedQg.totalResponses(2) mustEqual 3
      updatedQg.numCorrectResponses(2) mustEqual 1
    }

    "throw an exception on an attempt to get information from an unknown memory level" in {
      qg.totalResponses(9) must throwA[IndexOutOfBoundsException]
    }

    "have a count reset for a level" in {
      val memLevel2 = QuizGroupMemoryLevel(correctResponsesInARow = 2,
          repetitionInterval = 15, totalResponses = 10, numCorrectResponses = 1)
      val qg = QuizGroup(Map(2 -> memLevel2))
      val updatedQg = qg.incrementResponsesCorrect(2)
      updatedQg.totalResponses(2) mustEqual 1
      updatedQg.numCorrectResponses(2) mustEqual 1
    }
  }
}
