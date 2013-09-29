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

package com.oranda.libanius.model

import org.specs2.mutable.Specification
import com.oranda.libanius.dependencies.{AppDependencyAccess}
import com.oranda.libanius.model.quizitem.QuizItem

class QuizSpec extends Specification with AppDependencyAccess {
  
  "a quiz of word-mappings" should {
    
    val quizData = List(

        "quizGroup type=\"WordMapping\" promptType=\"English word\" responseType=\"German word\" isActive=\"true\" currentPromptNumber=\"0\"\n" +
        "against|wider\n" +
        "entertain|unterhalten\n" +
        "teach|unterrichten\n" +
        "winner|Siegerin\n" +
        "en route|unterwegs\n" +
        "full|satt/voll\n" +
        "interrupted|unterbrochen\n" +
        "contract|Vertrag\n" +
        "rides|reitet\n" +
        "on|auf\n" +
        "sweeps|streicht:100,200,300;405\n",

        "quizGroup type=\"WordMapping\" promptType=\"German word\" responseType=\"English word\" isActive=\"true\" currentPromptNumber=\"0\"\n" +
        "unterwegs|en route\n" +
        "Vertrag|contract:697,696;698/treaty:796;798")

    val quiz = Quiz.demoQuiz(quizData)

    "find values for a prompt" in {
      val quizLocal = Quiz.demoQuiz(quizData)
      val qgHeader = QuizGroupHeader(WordMapping, "English word", "German word")
      quizLocal.findResponsesFor("on", qgHeader) mustEqual List("auf")
    }
    
    "offer translations for a word, given the group of the word" in { 
      val translations = quiz.findResponsesFor(prompt = "Vertrag",
          QuizGroupHeader(WordMapping, "German word", "English word")).toSet[String]
      translations.contains("contract") mustEqual true
      translations.contains("treaty") mustEqual true
    }

    "add a new quiz item to a specified group" in {
      val quizBefore = Quiz.demoQuiz(quizData)
      val qgHeader = QuizGroupHeader(WordMapping, "English word", "German word")
      val newQuizItem = QuizItem("to exchange", "tauschen")
      val quizUpdated = quizBefore.addQuizItemToFront(qgHeader, newQuizItem)
      quizUpdated.existsQuizItem(newQuizItem, qgHeader) mustEqual true
    }

    "delete prompt-words from a particular group" in {
      val quizBefore = Quiz.demoQuiz(quizData)
      val wmgBefore = quizBefore.activeQuizGroups.get(
          QuizGroupHeader(WordMapping, "English word", "German word")).get
      wmgBefore.contains("full") mustEqual true
      val quizAfter = quizBefore.removeQuizItemsForPrompt("full",
          QuizGroupHeader(WordMapping, "English word", "German word"))
      val wmgAfter = quizAfter.activeQuizGroups.get(
          QuizGroupHeader(WordMapping, "English word", "German word")).get
      wmgAfter.contains("full") mustEqual false
    }

    "delete a quiz pair without deleting all values for that prompt" in {
      val quizBefore = Quiz.demoQuiz(quizData)
      val qgHeader = QuizGroupHeader(WordMapping, "German word", "English word")
      def translationsOfVertrag(quiz: Quiz) = quiz.findResponsesFor(prompt = "Vertrag", qgHeader)
      translationsOfVertrag(quizBefore).contains("contract") mustEqual true

      val (quizAfter, wasRemoved) = quizBefore.removeQuizItem(
          prompt = "Vertrag", response = "contract",
          QuizGroupHeader(WordMapping, "German word", "English word"))

      wasRemoved mustEqual true
      val translationsOfVertragAfter = translationsOfVertrag(quizAfter)
      translationsOfVertragAfter.contains("contract") mustEqual false
      translationsOfVertragAfter.contains("treaty") mustEqual true
    }

    "contain unique groups only" in {
      val quizLocal = Quiz.demoQuiz(quizData)
      quizLocal.numActiveGroups mustEqual 2 // precondition
      val qgHeader = QuizGroupHeader(WordMapping, "English word", "German word")
      val newQuizGroup =  QuizGroup(userData = QuizGroupUserData(isActive = true))
      val quizUpdated = quizLocal.addOrReplaceQuizGroup(qgHeader, newQuizGroup)
      quizUpdated.numActiveGroups mustEqual 2
    }

    "sum the number of correct answers" in {
      quiz.numCorrectAnswers mustEqual 6
    }

    "get the number of active quiz groups" in {
      quiz.numActiveGroups mustEqual 2
    }

    "activate and deactivate quiz groups" in {
      val quizLocal = Quiz.demoQuiz(quizData)
      val header = QuizGroupHeader(WordMapping, "English word", "German word")
      quizLocal.isActive(header) mustEqual true
      val quizAfterDeactivation = quizLocal.deactivate(header)
      quizAfterDeactivation.isActive(header) mustEqual false
      val quizAfterReactivation = quizAfterDeactivation.activate(header)
      quizAfterReactivation.isActive(header) mustEqual true
    }
  }
}