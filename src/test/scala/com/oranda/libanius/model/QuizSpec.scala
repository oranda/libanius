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
import com.oranda.libanius.model.quizitem.QuizItem
import com.oranda.libanius.model.quizgroup.{QuizGroupUserData, WordMapping, QuizGroupHeader, QuizGroup}

class QuizSpec extends Specification with AppDependencyAccess {
  
  "a quiz of word-mappings" should {
    
    val quizData = List(

        "quizGroup type=\"WordMapping\" promptType=\"English word\" responseType=\"German word\" isActive=\"true\" currentPromptNumber=\"0\"\n" +
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
        "sweeps|streicht|100,200,300;405\n",

        "quizGroup type=\"WordMapping\" promptType=\"German word\" responseType=\"English word\" isActive=\"true\" currentPromptNumber=\"0\"\n" +
        "unterwegs|en route\n" +
        "Vertrag|contract|697,696;698\n" +
        "Vertrag|treaty|796;798\n")

    val quiz = Quiz.demoQuiz(quizData)

    "find values for a prompt" in {
      quiz.numQuizItems mustEqual 15

      val qgHeader = QuizGroupHeader(WordMapping, "English word", "German word", "|")
      quiz.findResponsesFor("on", qgHeader) mustEqual List("auf")
    }
    
    "offer translations for a word, given the group of the word" in { 
      val translations = quiz.findResponsesFor(prompt = "Vertrag",
          QuizGroupHeader(WordMapping, "German word", "English word", "|")).toSet[String]
      translations.contains("contract") mustEqual true
      translations.contains("treaty") mustEqual true
    }

    "add a new quiz item to a specified group" in {
      val quizBefore = Quiz.demoQuiz(quizData)
      val qgHeader = QuizGroupHeader(WordMapping, "English word", "German word", "|")
      val quizUpdated = quizBefore.addQuizItemToFront(qgHeader, "to exchange", "tauschen")
      quizUpdated.existsQuizItem(QuizItem("to exchange", "tauschen"), qgHeader) mustEqual true
    }

    "delete a quiz pair without deleting all values for that prompt" in {
      val quizBefore = Quiz.demoQuiz(quizData)
      val qgHeader = QuizGroupHeader(WordMapping, "German word", "English word", "|")
      def translationsOfVertrag(quiz: Quiz) = quiz.findResponsesFor(prompt = "Vertrag", qgHeader)
      translationsOfVertrag(quizBefore).contains("contract") mustEqual true

      val (quizAfter, wasRemoved) = quizBefore.removeQuizItem(
          prompt = "Vertrag", response = "contract",
          QuizGroupHeader(WordMapping, "German word", "English word", "|"))

      wasRemoved mustEqual true
      val translationsOfVertragAfter = translationsOfVertrag(quizAfter)
      translationsOfVertragAfter.contains("contract") mustEqual false
      translationsOfVertragAfter.contains("treaty") mustEqual true
    }

    "contain unique groups only" in {
      quiz.numActiveGroups mustEqual 2 // precondition
      val qgHeader = QuizGroupHeader(WordMapping, "English word", "German word", "|")
      val newQuizGroup =  QuizGroup(QuizGroupUserData(isActive = true))
      val quizUpdated = quiz.addOrReplaceQuizGroup(qgHeader, newQuizGroup)
      quizUpdated.numActiveGroups mustEqual 2
    }

    "sum the number of correct answers" in {
      quiz.numCorrectAnswers mustEqual 6
    }

    "get the number of active quiz groups" in {
      quiz.numActiveGroups mustEqual 2
    }

    "activate and deactivate quiz groups" in {
      val header = QuizGroupHeader(WordMapping, "English word", "German word", "|")
      quiz.isActive(header) mustEqual true
      val quizAfterDeactivation = quiz.deactivate(header)
      quizAfterDeactivation.isActive(header) mustEqual false
      val quizAfterReactivation = quizAfterDeactivation.activate(header)
      quizAfterReactivation.isActive(header) mustEqual true
    }

    "update a quiz with a user response" in {
      quiz.numCorrectAnswers mustEqual 6
      val qgHeader = QuizGroupHeader(WordMapping, "English word", "German word", "|")
      val quizItem = QuizItem("against", "wider")
      val quizUpdated = quiz.updateWithUserResponse(true, qgHeader, quizItem)
      quizUpdated.numCorrectAnswers mustEqual 7
    }
  }
}