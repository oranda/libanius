/*
 * Libanius
 * Copyright (C) 2012-2022 James McCabe <jjtmccabe@gmail.com>
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

import com.oranda.libanius.dependencies.AppDependencyAccess
import com.oranda.libanius.model.TestData.*
import com.oranda.libanius.model.quizgroup.*
import com.oranda.libanius.model.quizitem.QuizItem
import org.specs2.mutable.Specification

class QuizSpec extends Specification with AppDependencyAccess {
  "a quiz" should {
    "have a certain number of active groups" in {
      quiz.numActiveGroups mustEqual 2
    }

    "have a certain number of quiz items" in {
      quiz.numQuizItems mustEqual 16
    }

    "find values for a prompt" in {
      quiz.findResponsesFor("on", qghEngGer.quizGroupKey) mustEqual List("auf")
    }

    "confirm a response is correct" in {
      quiz.isCorrect(quizGroupKeyEngGer, "en route", "unterwegs") mustEqual Correct
    }

    "confirm a response is incorrect" in {
      quiz.isCorrect(quizGroupKeyEngGer, "en route", "unterschrift") mustEqual Incorrect
    }

    "return NotFound for an isCorrect call on a nonexistent item" in {
      quiz.isCorrect(quizGroupKeyEngGer, "nonexistent-prompt", "unterschrift") mustEqual ItemNotFound
    }

    "find the quiz group header for a key" in {
      quiz.findQuizGroupHeader(quizGroupKeyEngGer) mustEqual Some(qghEngGer)
    }

    "find the quiz group for a key" in {
      quiz.findQuizGroup(quizGroupKeyEngGer).isDefined mustEqual true
    }

    "find the quiz group header for a prompt and response type" in {
      quiz.findQuizGroupHeader(quizGroupKeyEngGer) mustEqual Some(qghEngGer)
    }

    "find correct responses for a prompt" in {
      quiz.findResponsesFor("full", quizGroupKeyEngGer) mustEqual List("satt", "voll")
    }

    "find prompts corresponding to a response" in {
      quiz.findPromptsFor("satt", quizGroupKeyEngGer) mustEqual List("full")
    }

    "offer translations for a word, given the group of the word" in {
      val translations = quiz.findResponsesFor(prompt = "Vertrag", qghGerEng.quizGroupKey).toSet[String]
      translations.contains("contract") mustEqual true
      translations.contains("treaty") mustEqual true
    }

    "add a new quiz item to a specified group" in {
      val quizBefore  = Quiz.demoQuiz(quizText)
      val quizItem    = QuizItem("to exchange", "tauschen")
      val quizUpdated = quizBefore.addQuizItemToFront(quizItem, qghEngGer)
      quizUpdated.existsQuizItem(quizItem, qghEngGer) mustEqual true
    }

    "delete a quiz pair without deleting all values for that prompt" in {
      val quizBefore = Quiz.demoQuiz(quizText)
      def translationsOfVertrag(quiz: Quiz) =
        quiz.findResponsesFor(prompt = "Vertrag", qghGerEng.quizGroupKey)
      translationsOfVertrag(quizBefore).contains("contract") mustEqual true

      val (quizAfter, wasRemoved) =
        quizBefore.removeQuizItem(prompt = "Vertrag", response = "contract", qghGerEng)

      wasRemoved mustEqual true
      val translationsOfVertragAfter = translationsOfVertrag(quizAfter)
      translationsOfVertragAfter.contains("contract") mustEqual false
      translationsOfVertragAfter.contains("treaty") mustEqual true
    }

    "contain unique groups only" in {
      quiz.numActiveGroups mustEqual 2 // precondition
      val isActive     = true
      val newQuizGroup = QuizGroup.fromQuizItems(LazyList.empty, 6, QuizGroupUserData(isActive))
      val quizUpdated  = quiz.addOrReplaceQuizGroup(qghEngGer, newQuizGroup)
      quizUpdated.numActiveGroups mustEqual 2
    }

    "sum the number of correct answers" in {
      quiz.numCorrectResponses mustEqual 6
    }

    "get the number of active quiz groups" in {
      quiz.numActiveGroups mustEqual 2
    }

    "activate and deactivate quiz groups" in {
      quiz.isActive(qghEngGer) mustEqual true
      val quizAfterDeactivation = quiz.deactivate(qghEngGer)
      quizAfterDeactivation.isActive(qghEngGer) mustEqual false
      val quizAfterReactivation = quizAfterDeactivation.activate(qghEngGer)
      quizAfterReactivation.isActive(qghEngGer) mustEqual true
    }

    "deactivate all quiz groups" in {
      quiz.isActive(qghEngGer) mustEqual true
      quiz.isActive(qghGerEng) mustEqual true

      val quizAfterDeactivation = quiz.deactivateAll

      quizAfterDeactivation.isActive(qghEngGer) mustEqual false
      quizAfterDeactivation.isActive(qghGerEng) mustEqual false
    }

    "get a quiz with one group" in {
      val quiz = Quiz.getQuizWithOneGroup("English word", "German word")
      quiz.numActiveGroups mustEqual 1
    }

    "get a default quiz" in {
      val quiz = Quiz.getDefaultQuiz
      quiz.numActiveGroups mustEqual 1
      quiz.findQuizGroupHeader(quizGroupKeyEngGer) mustEqual Some(qghEngGer)
    }

    "update a quiz with a user response" in {
      quiz.numCorrectResponses mustEqual 6

      quiz.numCorrectResponses(qghEngGer, 0) mustEqual 0
      val quizItem    = QuizItem("against", "wider")
      val quizUpdated = quiz.updateWithUserResponse(isCorrect = true, qghEngGer, quizItem)
      quizUpdated.numCorrectResponses mustEqual 7
      quizUpdated.numCorrectResponses(qghEngGer, 0) mustEqual 1
    }

    def updateWithResponsesFor(
      quiz: Quiz,
      wasCorrect: Boolean,
      qgHeader: QuizGroupHeader,
      quizPrompts: List[String]
    ): Quiz = {

      var quizUpdated                 = quiz
      def quizItemFor(prompt: String) = quizUpdated.findQuizItem(qgHeader, prompt).get

      quizPrompts.foreach { quizPrompt =>
        quizUpdated = quizUpdated.updateWithUserResponse(wasCorrect, qgHeader, quizItemFor(quizPrompt))
      }
      quizUpdated
    }

    val quizPromptsFor11Items =
      List(
        "against",
        "entertain",
        "teach",
        "winner",
        "en route",
        "full",
        "interrupted",
        "contract",
        "rides",
        "on",
        "the"
      )

    val demoQuizWith1stMemoryLevelIntervalOf5 = Quiz.demoQuiz(quizText)

    "narrow a repetition interval after bad user performance" in {

      demoQuizWith1stMemoryLevelIntervalOf5.memoryLevelInterval(qghEngGer, 1) mustEqual 5

      val quizUpdated1 = updateWithResponsesFor(
        demoQuizWith1stMemoryLevelIntervalOf5,
        wasCorrect = true,
        qghEngGer,
        quizPromptsFor11Items
      )
      val quizUpdated2 = updateWithResponsesFor(quizUpdated1, wasCorrect = false, qghEngGer, quizPromptsFor11Items)
      quizUpdated2.memoryLevelInterval(qghEngGer, 1) mustEqual 4
    }

    "widen a repetition interval after good user performance" in {
      demoQuizWith1stMemoryLevelIntervalOf5.memoryLevelInterval(qghEngGer, 1) mustEqual 5

      val quizUpdated1 = updateWithResponsesFor(
        demoQuizWith1stMemoryLevelIntervalOf5,
        wasCorrect = true,
        qghEngGer,
        quizPromptsFor11Items
      )
      val quizUpdated2 = updateWithResponsesFor(quizUpdated1, wasCorrect = true, qghEngGer, quizPromptsFor11Items)
      quizUpdated2.memoryLevelInterval(qghEngGer, 1) mustEqual 6
    }
  }
}
