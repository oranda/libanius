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

package com.oranda.libanius.model

import org.specs2.mutable.Specification
import com.oranda.libanius.dependencies.{AppDependencyAccess}
import com.oranda.libanius.model.quizitem.{TextValue, QuizItem}
import com.oranda.libanius.model.quizgroup.{QuizGroupUserData, WordMapping, QuizGroupHeader, QuizGroup}

class QuizSpec extends Specification with AppDependencyAccess {
  
  "a quiz" should {
    
    val quizData = List(

        "#quizGroup type=\"WordMapping\" promptType=\"English word\" responseType=\"German word\" isActive=\"true\" currentPromptNumber=\"0\"\n" +
        "#quizGroupPartition numCorrectResponsesInARow=\"0\" repetitionInterval=\"0\"\n" +
        "against|wider\n" +
        "entertain|unterhalten\n" +
        "teach|unterrichten\n" +
        "winner|Siegerin\n" +
        "en route|unterwegs\n" +
        "full|satt\n" +
        "full|voll\n" +
        "interrupted|unterbrochen\n" +
        "contract|Vertrag\n" +
        "rides|reitet\n" +
        "on|auf\n" +
        "the|der\n" +
        "#quizGroupPartition numCorrectResponsesInARow=\"3\" repetitionInterval=\"15\"\n" +
        "sweeps|streicht|100,200,300;405\n",

        "#quizGroup type=\"WordMapping\" promptType=\"German word\" responseType=\"English word\" isActive=\"true\" currentPromptNumber=\"0\"\n" +
        "#quizGroupPartition numCorrectResponsesInARow=\"0\" repetitionInterval=\"0\"\n" +
        "unterwegs|en route\n" +
        "#quizGroupPartition numCorrectResponsesInARow=\"1\" repetitionInterval=\"5\"\n" +
        "Vertrag|treaty|796;798\n" +
        "#quizGroupPartition numCorrectResponsesInARow=\"2\" repetitionInterval=\"15\"\n" +
        "Vertrag|contract|697,696;698\n"
    )

    val quiz = Quiz.demoQuiz(quizData)
    val qghEngGer = QuizGroupHeader(WordMapping, "English word", "German word", "|", 4)
    val qghGerEng = QuizGroupHeader(WordMapping, "German word", "English word", "|", 4)

    "be parseable from custom format" in {
      quiz.numActiveGroups mustEqual 2
      quiz.numQuizItems mustEqual 16
    }

    "find values for a prompt" in {
      quiz.findResponsesFor("on", qghEngGer) mustEqual List("auf")
    }
    
    "offer translations for a word, given the group of the word" in { 
      val translations = quiz.findResponsesFor(prompt = "Vertrag", qghGerEng).toSet[String]
      translations.contains("contract") mustEqual true
      translations.contains("treaty") mustEqual true
    }

    "add a new quiz item to a specified group" in {
      val quizBefore = Quiz.demoQuiz(quizData)
      val quizUpdated = quizBefore.addQuizItemToFront(qghEngGer, "to exchange", "tauschen")
      quizUpdated.existsQuizItem(QuizItem("to exchange", "tauschen"), qghEngGer) mustEqual true
    }

    "delete a quiz pair without deleting all values for that prompt" in {
      val quizBefore = Quiz.demoQuiz(quizData)
      def translationsOfVertrag(quiz: Quiz) = quiz.findResponsesFor(prompt = "Vertrag", qghGerEng)
      translationsOfVertrag(quizBefore).contains("contract") mustEqual true

      val (quizAfter, wasRemoved) = quizBefore.removeQuizItem(
          prompt = "Vertrag", response = "contract", qghGerEng)

      wasRemoved mustEqual true
      val translationsOfVertragAfter = translationsOfVertrag(quizAfter)
      translationsOfVertragAfter.contains("contract") mustEqual false
      translationsOfVertragAfter.contains("treaty") mustEqual true
    }

    "contain unique groups only" in {
      quiz.numActiveGroups mustEqual 2 // precondition
      val newQuizGroup =  QuizGroup.fromQuizItems(Stream.empty, QuizGroupUserData(isActive = true))
      val quizUpdated = quiz.addOrReplaceQuizGroup(qghEngGer, newQuizGroup)
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

    "update a quiz with a user response" in {
      quiz.numCorrectResponses mustEqual 6

      quiz.numCorrectResponses(qghEngGer, 0) mustEqual 0
      val quizItem = QuizItem("against", "wider")
      val quizUpdated = quiz.updateWithUserResponse(true, qghEngGer, quizItem)
      quizUpdated.numCorrectResponses mustEqual 7
      quizUpdated.numCorrectResponses(qghEngGer, 0) mustEqual 1
    }

    "find a presentable quiz item" in {
      val (quizItemViewWithChoices, _) = quiz.findPresentableQuizItem.get
      quizItemViewWithChoices.prompt mustEqual TextValue("against")
    }

    def updateWithResponsesFor(quiz: Quiz, wasCorrect: Boolean,
        qgHeader: QuizGroupHeader, quizPrompts: List[String]): Quiz = {

      var quizUpdated = quiz
      def quizItemFor(prompt: String) = quizUpdated.findQuizItem(qgHeader, prompt).get

      quizPrompts.foreach { quizPrompt =>
        quizUpdated = quizUpdated.updateWithUserResponse(wasCorrect, qgHeader, quizItemFor(quizPrompt))
      }
      quizUpdated
    }

    val quizPromptsFor11Items =
      List("against", "entertain", "teach", "winner", "en route", "full",
          "interrupted", "contract", "rides", "on", "the")

    val demoQuizWith1stMemoryLevelIntervalOf5 = Quiz.demoQuiz(quizData)

    "narrow a repetition interval after bad user performance" in {

      demoQuizWith1stMemoryLevelIntervalOf5.memoryLevelInterval(qghEngGer, 1) mustEqual 5

      val quizUpdated1 = updateWithResponsesFor(demoQuizWith1stMemoryLevelIntervalOf5,
          wasCorrect = true, qghEngGer, quizPromptsFor11Items)
      val quizUpdated2 = updateWithResponsesFor(quizUpdated1,
          wasCorrect = false, qghEngGer, quizPromptsFor11Items)
      quizUpdated2.memoryLevelInterval(qghEngGer, 1) mustEqual 4
    }

    "widen a repetition interval after good user performance" in {
      demoQuizWith1stMemoryLevelIntervalOf5.memoryLevelInterval(qghEngGer, 1) mustEqual 5

      val quizUpdated1 = updateWithResponsesFor(demoQuizWith1stMemoryLevelIntervalOf5,
          wasCorrect = true, qghEngGer, quizPromptsFor11Items)
      val quizUpdated2 = updateWithResponsesFor(quizUpdated1,
          wasCorrect = true, qghEngGer, quizPromptsFor11Items)
      quizUpdated2.memoryLevelInterval(qghEngGer, 1) mustEqual 6
    }
  }
}