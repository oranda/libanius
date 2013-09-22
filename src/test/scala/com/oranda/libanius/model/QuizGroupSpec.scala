/* Copyright 2012-2013 James McCabe <james@oranda.com>
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
import com.oranda.libanius.model.quizitem.{QuizItem, QuizItemViewWithChoices, TextValue}

import java.lang.StringBuilder

class QuizGroupSpec extends Specification with AppDependencyAccess {

  "a quiz group" should {

    val wmgCustomFormat =
      "quizGroup type=\"WordMapping\" promptType=\"English word\" responseType=\"German word\" currentPromptNumber=\"0\"\n" +
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
        "sweeps|streicht"


    def makeQuizGroup: QuizGroupWithHeader = QuizGroup.fromCustomFormat(wmgCustomFormat)

    // defaults for read-only
    val qgWithHeader = makeQuizGroup
    val quizGroup = qgWithHeader.quizGroup
    val qgHeader = qgWithHeader.header

    "be parseable from custom format" in {
      quizGroup.currentPromptNumber mustEqual 0
      qgHeader.promptType mustEqual "English word"
      qgHeader.responseType mustEqual "German word"
      quizGroup.toCustomFormat(new StringBuilder(), qgHeader).toString mustEqual wmgCustomFormat
      quizGroup.size mustEqual 12
    }

    "find values for a prompt" in {
      quizGroup.findResponsesFor("on") mustEqual List("auf")
    }

    "accept an updated prompt number" in {
      val qgLocal = makeQuizGroup
      val qgUpdated = qgLocal.quizGroup.updatedPromptNumber
      qgUpdated.currentPromptNumber mustEqual 1
    }

    "accept the addition of a new word-mapping" in {
      val qgLocal = makeQuizGroup
      qgLocal.quizGroup.contains("good") mustEqual false
      val qgUpdated = qgLocal.quizGroup.addNewQuizItem("good", "gut")
      qgUpdated.contains("good") mustEqual true
    }

    "accept new values for an existing word-mapping" in {
      val qgLocal = makeQuizGroup
      val valuesForAgainst = qgLocal.quizGroup.findResponsesFor("against")
      valuesForAgainst.size mustEqual 1
      val qgUpdated = qgLocal.quizGroup.addQuizItem(TextValue("against"), TextValue("gegen"))
      qgUpdated.findResponsesFor("against").size mustEqual 2
    }

    "generate false answers similar to a correct answer" in {
      val falseAnswers = quizGroup.makeFalseSimilarAnswers(
          correctValues = List("unterhalten"),
          correctValue = "unterhalten",
          numCorrectAnswersSoFar = 2, numFalseAnswersRequired = 5)

      falseAnswers.contains("unterrichten") mustEqual true
    }

    "make false answers" in {
      val quizItemCorrect: QuizItem = quizGroup.quizItems.find(_.prompt.value == "entertain").get

      val falseAnswers = quizGroup.makeFalseAnswers(quizItemCorrect: QuizItem,
          numCorrectAnswersSoFar = 2)

      falseAnswers.contains("unterrichten") mustEqual true
    }

    def pullQuizItem(header: QuizGroupHeader, qg: QuizGroup): (QuizGroup, (String, String)) = {
      val quizItem = qg.findPresentableQuizItem(header)
      quizItem.isDefined mustEqual true
      // Each time a quiz item is pulled, a user answer must be set
      val qgUpdated = qg.updatedWithUserAnswer(quizItem.get.prompt,
          quizItem.get.response, true, UserResponses(), new UserResponse(0))
      (qgUpdated, (quizItem.get.prompt.value, quizItem.get.response.value))
    }

    "find a presentable quiz item" in {
      pullQuizItem(qgHeader, quizGroup)._2 mustEqual ("against", "wider")
    }

    "remove a quiz pair" in {
      val qgLocal = makeQuizGroup
      val itemToRemove = QuizItem("against", "wider")
      val qgUpdated = qgLocal.quizGroup.removeQuizItem(itemToRemove)
      qgUpdated.contains("against") mustEqual false
    }

    "add a new quiz item to the front of its queue" in {
      val qgLocal = makeQuizGroup
      val qgUpdated = qgLocal.quizGroup.addNewQuizItem("to exchange", "tauschen")
      pullQuizItem(qgLocal.header, qgUpdated)._2 mustEqual ("to exchange", "tauschen")
    }

    "move an existing quiz pair to the front of its queue" in {
      val qgLocal = makeQuizGroup
      val numPromptsBefore = qgLocal.quizGroup.numPrompts
      val qgUpdated = qgLocal.quizGroup.addQuizItemToFront(QuizItem("sweeps", "streicht"))
      val numPromptsAfter = qgUpdated.numPrompts
      numPromptsAfter mustEqual numPromptsBefore
      pullQuizItem(qgLocal.header, qgUpdated)._2 mustEqual ("sweeps", "streicht")
    }

    "move a quiz pair to the front of its queue where only the prompt already exists" in {
      val qgLocal = makeQuizGroup
      val sizeBefore = qgLocal.quizGroup.size
      val qgUpdated = qgLocal.quizGroup.addQuizItemToFront(QuizItem("entertain", "bewirten"))
      val sizeAfter = qgUpdated.size
      sizeAfter mustEqual sizeBefore + 1
      pullQuizItem(qgLocal.header, qgUpdated)._2 mustEqual ("entertain", "bewirten")
    }

    "add more than one new quiz pair to the front of its queue" in {
      val qgLocal = makeQuizGroup
      val qgUpdated1 = qgLocal.quizGroup.addQuizItemToFront(QuizItem("to exchange", "tauschen"))
      val qgUpdated2 = qgUpdated1.addQuizItemToFront(QuizItem("whole", "ganz"))
      val (qgUnrolled, (keyWord, value)) = pullQuizItem(qgLocal.header, qgUpdated2)
      (keyWord, value) mustEqual ("whole", "ganz")
      pullQuizItem(qgLocal.header, qgUnrolled)._2 mustEqual ("to exchange", "tauschen")
    }

    def pullQuizItemAndAnswerCorrectly(qgh: QuizGroupWithHeader): QuizGroupWithHeader = {
      val quizItem = qgh.quizGroup.findPresentableQuizItem(qgh.header).get
      QuizGroupWithHeader(qgh.header, updateWithUserAnswer(qgh.quizGroup, quizItem))
    }

    def updateWithUserAnswer(qg: QuizGroup, quizItem: QuizItemViewWithChoices): QuizGroup = {
      val userAnswer = new UserResponse(qg.currentPromptNumber)
      qg.updatedWithUserAnswer(quizItem.prompt, quizItem.response, true, UserResponses(),
          userAnswer).updatedPromptNumber
    }

    "present an item that has been answered before after five prompts" in {
      var qgLocal = makeQuizGroup
      val quizItem0 = qgLocal.quizGroup.findPresentableQuizItem(qgLocal.header).get
      quizItem0.prompt.value mustEqual "against"
      qgLocal = QuizGroupWithHeader(qgLocal.header,
          updateWithUserAnswer(qgLocal.quizGroup, quizItem0))

      for (promptNum <- 1 until 5)
        qgLocal = pullQuizItemAndAnswerCorrectly(qgLocal)

      val quizItem5 = qgLocal.quizGroup.findPresentableQuizItem(qgLocal.header)
      quizItem5.get.prompt.value mustEqual "against"
    }

  }


}
