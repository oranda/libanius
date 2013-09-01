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
import com.oranda.libanius.dependencies.{Conf, AppDependencies}
import com.oranda.libanius.model.wordmapping.{WordMappingValueSetWrapper, WordMappingValueSet, WordMappingGroup}

class QuizGroupSpec extends Specification {

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
        "sweeps|streicht"

    AppDependencies.conf = Conf.setUpForTest()

    val qg = QuizGroup.fromCustomFormat(wmgCustomFormat)

    "be parseable from custom format" in {
      qg.currentPromptNumber mustEqual 0
      qg.promptType mustEqual "English word"
      qg.responseType mustEqual "German word"
      qg.toCustomFormat(new StringBuilder()).toString mustEqual wmgCustomFormat
      qg.size mustEqual 11
    }

    "accept the addition of a new word-mapping" in {
      val qgLocal = QuizGroup.fromCustomFormat(wmgCustomFormat)
      qgLocal.contains("good") mustEqual false
      val qgUpdated = qgLocal.addNewQuizItem("good", "gut")
      qgUpdated.contains("good") mustEqual true
    }

    "accept new values for an existing word-mapping" in {
      val qgLocal = QuizGroup.fromCustomFormat(wmgCustomFormat)
      val valuesForAgainst = qgLocal.findValuesFor("against")
      valuesForAgainst.size mustEqual 1
      val qgUpdated = qgLocal.addQuizItem(TextValue("against"), TextValue("gegen"))
      qgUpdated.findValuesFor("against").size mustEqual 2
    }

    "generate false answers similar to a correct answer" in {
      val falseAnswers = qg.makeFalseSimilarAnswers(
          correctValues = List("unterhalten"),
          correctValue = "unterhalten",
          numCorrectAnswersSoFar = 2, numFalseAnswersRequired = 5)
      falseAnswers.contains("unterrichten") mustEqual true
    }

    def pullQuizItem(qg: QuizGroup): (QuizGroup, (String, String)) = {
      val quizItem = qg.findPresentableQuizItem
      quizItem.isDefined mustEqual true
      // Each time a quiz item is pulled, a user answer must be set
      val qgUpdated = qg.updatedWithUserAnswer(quizItem.get.prompt,
          quizItem.get.response, true, UserResponses(), new UserResponse(0))
      (qgUpdated, (quizItem.get.prompt.text, quizItem.get.response.text))
    }

    "find a presentable quiz item" in {
      pullQuizItem(qg)._2 mustEqual ("against", "wider")
    }

    "remove a quiz pair" in {
      val qgLocal = QuizGroup.fromCustomFormat(wmgCustomFormat)
      val itemToRemove = QuizItem("against", "wider")
      val qgUpdated = qgLocal.removeQuizItem(itemToRemove)
      qgUpdated.contains("against") mustEqual false
    }

    "add a new quiz pair to the front of its queue" in {
      val qgLocal = QuizGroup.fromCustomFormat(wmgCustomFormat)
      val qgUpdated = qgLocal.addNewQuizItem("to exchange", "tauschen")
      pullQuizItem(qgUpdated)._2 mustEqual ("to exchange", "tauschen")
    }

    "move an existing quiz pair to the front of its queue" in {
      val qgLocal = QuizGroup.fromCustomFormat(wmgCustomFormat)
      val numPromptsBefore = qgLocal.numPrompts
      val qgUpdated = qgLocal.addQuizItemToFront(TextValue("sweeps"), TextValue("streicht"))
      val numPromptsAfter = qgUpdated.numPrompts
      numPromptsAfter mustEqual numPromptsBefore
      pullQuizItem(qgUpdated)._2 mustEqual ("sweeps", "streicht")
    }

    "move a quiz pair to the front of its queue where only the prompt already exists" in {
      val qgLocal = QuizGroup.fromCustomFormat(wmgCustomFormat)
      val sizeBefore = qgLocal.size
      val qgUpdated = qgLocal.addQuizItemToFront(TextValue("entertain"), TextValue("bewirten"))
      val sizeAfter = qgUpdated.size
      sizeAfter mustEqual sizeBefore + 1
      pullQuizItem(qgUpdated)._2 mustEqual ("entertain", "bewirten")
    }

    "add more than one new quiz pair to the front of its queue" in {
      val qgLocal = QuizGroup.fromCustomFormat(wmgCustomFormat)
      val qgUpdated1 = qgLocal.addQuizItemToFront(TextValue("to exchange"),
          TextValue("tauschen"))
      val qgUpdated2 = qgUpdated1.addQuizItemToFront(TextValue("whole"), TextValue("ganz"))
      val (qgUnrolled, (keyWord, value)) = pullQuizItem(qgUpdated2)
      (keyWord, value) mustEqual ("whole", "ganz")
      pullQuizItem(qgUnrolled)._2 mustEqual ("to exchange", "tauschen")
    }

    def pullQuizItemAndAnswerCorrectly(qg: QuizGroup): QuizGroup = {
      val quizItem = qg.findPresentableQuizItem.get
      updateWithUserAnswer(qg, quizItem)
    }

    def updateWithUserAnswer(qg: QuizGroup, quizItem: QuizItemViewWithChoices) = {
      val userAnswer = new UserResponse(qg.currentPromptNumber)
      qg.updatedWithUserAnswer(quizItem.prompt, quizItem.response, true, UserResponses(),
          userAnswer).updatedPromptNumber
    }

    "present an item that has been answered before after five prompts" in {
      var qgLocal = QuizGroup.fromCustomFormat(wmgCustomFormat)
      val quizItem0 = qgLocal.findPresentableQuizItem.get
      quizItem0.prompt.text mustEqual "against"
      qgLocal = updateWithUserAnswer(qgLocal, quizItem0)

      for (promptNum <- 1 until 5)
        qgLocal = pullQuizItemAndAnswerCorrectly(qgLocal)

      val quizItem5 = qgLocal.findPresentableQuizItem
      quizItem5.get.prompt.text mustEqual "against"
    }
  }


}
