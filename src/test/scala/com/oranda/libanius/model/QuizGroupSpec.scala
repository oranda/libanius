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
      "quizGroup type=\"WordMapping\" cueType=\"English word\" responseType=\"German word\" currentPromptNumber=\"0\"\n" +
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
      qg.cueType mustEqual "English word"
      qg.responseType mustEqual "German word"
      qg.toCustomFormat(new StringBuilder()).toString mustEqual wmgCustomFormat
      qg.size mustEqual 11
    }

    "accept the addition of a new word-mapping" in {
      val qgLocal = QuizGroup.fromCustomFormat(wmgCustomFormat)
      qgLocal.contains("good") mustEqual false
      val qgUpdated = qgLocal.addQuizPair("good", "gut")
      qgUpdated.contains("good") mustEqual true
    }

    "accept new values for an existing word-mapping" in {
      val qgLocal = QuizGroup.fromCustomFormat(wmgCustomFormat)
      val valuesForAgainst = qgLocal.findValuesFor("against")
      valuesForAgainst.size mustEqual 1
      val qgUpdated = qgLocal.addQuizPair("against", "gegen")
      qgUpdated.findValuesFor("against").size mustEqual 2
    }

    "generate false answers similar to a correct answer" in {
      val falseAnswers = qg.makeFalseSimilarAnswers(
        correctQuizValues = List(new QuizValueWithUserAnswers("unterhalten")),
        correctValue = new QuizValueWithUserAnswers("unterhalten"),
        numCorrectAnswersSoFar = 2, numFalseAnswersRequired = 5)
      falseAnswers.contains("unterrichten") mustEqual true
    }

    def pullQuizItem(qg: QuizGroup): (QuizGroup, (String, String)) = {
      val quizItem = qg.findPresentableQuizItem
      quizItem.isDefined mustEqual true
      // Each time a quiz item is pulled, a user answer must be set
      val qgUpdated = qg.updatedWithUserAnswer(quizItem.get.cue,
          quizItem.get.quizValue, new UserAnswer(true, 0))
      (qgUpdated, (quizItem.get.cue, quizItem.get.quizValue.value))
    }

    "find a presentable quiz item" in {
      pullQuizItem(qg)._2 mustEqual ("against", "wider")
    }

    "add a new quiz pair to the front of its queue" in {
      val qgLocal = QuizGroup.fromCustomFormat(wmgCustomFormat)
      val qgUpdated = qgLocal.addQuizPairToFront("to exchange", "tauschen")
      pullQuizItem(qgUpdated)._2 mustEqual ("to exchange", "tauschen")
    }

    "move an existing quiz pair to the front of its queue" in {
      val qgLocal = QuizGroup.fromCustomFormat(wmgCustomFormat)
      val numKeyWordsBefore = qgLocal.numCues
      val qgUpdated = qgLocal.addQuizPairToFront("sweeps", "streicht")
      val numKeyWordsAfter = qgUpdated.numCues
      numKeyWordsAfter mustEqual numKeyWordsBefore
      pullQuizItem(qgUpdated)._2 mustEqual ("sweeps", "streicht")
    }

    "move a quiz pair to the front of its queue where only the cue already exists" in {
      val qgLocal = QuizGroup.fromCustomFormat(wmgCustomFormat)
      val sizeBefore = qgLocal.size
      val qgUpdated = qgLocal.addQuizPairToFront("entertain", "bewirten")
      val sizeAfter = qgUpdated.numCues
      sizeAfter mustEqual sizeBefore + 1
      pullQuizItem(qgUpdated)._2 mustEqual ("entertain", "bewirten")
    }

    "add more than one new quiz pair to the front of its queue" in {
      val qgLocal = QuizGroup.fromCustomFormat(wmgCustomFormat)
      val qgUpdated1 = qgLocal.addQuizPairToFront("to exchange", "tauschen")
      val qgUpdated2 = qgUpdated1.addQuizPairToFront("whole", "ganz")
      val (qgUnrolled, (keyWord, value)) = pullQuizItem(qgUpdated2)
      (keyWord, value) mustEqual ("whole", "ganz")
      pullQuizItem(qgUnrolled)._2 mustEqual ("to exchange", "tauschen")
    }

    def pullQuizItemAndAnswerCorrectly(qg: QuizGroup): QuizGroup = {
      val quizItem = qg.findPresentableQuizItem.get
      updateWithUserAnswer(qg, quizItem)
    }

    def updateWithUserAnswer(qg: QuizGroup, quizItem: QuizItemViewWithChoices) = {
      val userAnswer = new UserAnswer(true, qg.currentPromptNumber)
      qg.updatedWithUserAnswer(quizItem.cue, quizItem.quizValue, userAnswer).
          updatedPromptNumber
    }

    "present an item that has been answered before after five prompts" in {
      var qgLocal = QuizGroup.fromCustomFormat(wmgCustomFormat)
      val quizItem0 = qgLocal.findPresentableQuizItem.get
      quizItem0.quizValue.value mustEqual "wider"
      qgLocal = updateWithUserAnswer(qgLocal, quizItem0)

      for (promptNum <- 1 until 5)
        qgLocal = pullQuizItemAndAnswerCorrectly(qgLocal)

      val quizItem5 = qgLocal.findPresentableQuizItem
      quizItem5.get.quizValue.value mustEqual "wider"
    }
  }


}
