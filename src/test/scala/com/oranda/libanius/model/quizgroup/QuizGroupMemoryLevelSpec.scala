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

package com.oranda.libanius.model.quizgroup

import org.specs2.mutable.Specification
import com.oranda.libanius.dependencies.AppDependencyAccess
import com.oranda.libanius.model.quizitem.{QuizItem, TextValue}

import com.oranda.libanius.model._
import com.oranda.libanius.model.UserResponse
import TestData._

import com.oranda.libanius.model.action.producequizitem._
import ProduceQuizItem._
import ProduceQuizItemForModelComponents._
import ProduceQuizItemSpec._

class QuizGroupMemoryLevelSpec extends Specification with AppDependencyAccess {

  "a quiz group memory level" should {

    "find values for a prompt" in {
      qgMemLevel.findResponsesFor("on") mustEqual List("auf")
    }

    "accept the addition of a new word-mapping" in {
      qgMemLevel.contains("good") mustEqual false
      val qgUpdated = qgMemLevel.addNewQuizItem("good", "gut")
      qgUpdated.contains("good") mustEqual true
    }

    "accept new values for an existing word-mapping" in {
      val valuesForAgainst = qgMemLevel.findResponsesFor("against")
      valuesForAgainst.size mustEqual 1
      val qgUpdated = qgMemLevel.addQuizItem(TextValue("against"), TextValue("gegen"))
      qgUpdated.findResponsesFor("against").size mustEqual 2
    }

    "generate false answers similar to a correct answer" in {
      def hasSameEnd = (value1: TextValue, value2: String) => value1.hasSameEnd(value2)

      val falseAnswers = qgMemLevel.constructWrongChoicesSimilar(
        correctResponses = List("unterhalten"),
        numWrongResponsesRequired = 5,
        correctValue = "unterhalten",
        similarityPredicate = hasSameEnd)

      falseAnswers.contains("unterrichten") mustEqual true
    }

    "remove a quiz pair" in {
      val itemToRemove = QuizItem("against", "wider")
      val qgUpdated = qgMemLevel.removeQuizItem(itemToRemove)
      qgUpdated.contains("against") mustEqual false
    }

    "add a new quiz item to the front of its queue" in {
      val qgUpdated = qgMemLevel.addNewQuizItem("to exchange", "tauschen")
      pullQuizItem(qgUpdated, 0)._2 mustEqual ("to exchange", "tauschen")
    }

    "move an existing quiz pair to the front of its queue" in {
      val numPromptsBefore = qgMemLevel.numPrompts
      val qgUpdated = qgMemLevel.addQuizItemToFront(QuizItem("sweeps", "streicht"))
      val numPromptsAfter = qgUpdated.numPrompts
      numPromptsAfter mustEqual numPromptsBefore
      pullQuizItem(qgUpdated, 0)._2 mustEqual ("sweeps", "streicht")
    }

    "move a quiz pair to the front of its queue where only the prompt already exists" in {
      val sizeBefore = qgMemLevel.size
      val qgUpdated = qgMemLevel.addQuizItemToFront(QuizItem("entertain", "bewirten"))
      val sizeAfter = qgUpdated.size
      sizeAfter mustEqual sizeBefore + 1
      pullQuizItem(qgUpdated, 0)._2 mustEqual ("entertain", "bewirten")
    }

    "add more than one new quiz pair to the front of its queue" in {
      val qgUpdated1 = qgMemLevel.addQuizItemToFront(QuizItem("to exchange", "tauschen"))
      val qgUpdated2 = qgUpdated1.addQuizItemToFront(QuizItem("whole", "ganz"))
      val (qgUnrolled, (keyWord, value)) = pullQuizItem(qgUpdated2, 0)
      (keyWord, value) mustEqual ("whole", "ganz")
      pullQuizItem(qgUnrolled, 1)._2 mustEqual ("to exchange", "tauschen")
    }

  }
}
