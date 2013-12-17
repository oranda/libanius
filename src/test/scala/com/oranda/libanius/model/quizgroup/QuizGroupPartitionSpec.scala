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

package com.oranda.libanius.model.quizgroup

import org.specs2.mutable.Specification
import com.oranda.libanius.dependencies.AppDependencyAccess
import com.oranda.libanius.model.quizitem.{QuizItem, TextValue}

import com.oranda.libanius.model.{UserResponse, UserResponses}
import java.lang.StringBuilder

class QuizGroupPartitionSpec extends Specification with AppDependencyAccess {

  "a quiz group partition" should {
    /*
    val wmgCustomFormat =
      "quizGroup type=\"WordMapping\" promptType=\"English word\" responseType=\"German word\" currentPromptNumber=\"0\" isActive=\"true\"\n" +
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

    Construct a quiz group partition.
    */
    def makeQgPartition: QuizGroupPartition = QuizGroupPartition(List(
        QuizItem("against", "wider"),
        QuizItem("entertain", "unterhalten"),
        QuizItem("teach", "unterrichten"),
        QuizItem("winner", "Siegerin"),
        QuizItem("en route", "unterwegs"),
        QuizItem("full", "satt"),
        QuizItem("full", "voll"),
        QuizItem("interrupted", "unterbrochen"),
        QuizItem("contract", "Vertrag"),
        QuizItem("rides", "reitet"),
        QuizItem("on", "auf"),
        QuizItem("sweeps", "streicht")).toStream)

    def makeQgPartitionSimple: QuizGroupPartition = QuizGroupPartition(List(
        QuizItem("against", "wider"),
        QuizItem("entertain", "unterhalten")).toStream)


    // defaults for read-only
    val qgPartition = makeQgPartition
    val qgPartitionSimple = makeQgPartitionSimple

    val qgPartitionSimpleCustomFormat =
        "against|wider|\n" +
        "entertain|unterhalten|\n"


    "be serializable to custom format" in {
      val customFormat = qgPartitionSimple.toCustomFormat(new StringBuilder(), "|")
      customFormat.toString mustEqual qgPartitionSimpleCustomFormat
    }

    "find values for a prompt" in {
      qgPartition.findResponsesFor("on") mustEqual List("auf")
    }

    "accept the addition of a new word-mapping" in {
      val qgpLocal = makeQgPartition
      qgpLocal.contains("good") mustEqual false
      val qgUpdated = qgpLocal.addNewQuizItem("good", "gut")
      qgUpdated.contains("good") mustEqual true
    }

    "accept new values for an existing word-mapping" in {
      val qgpLocal = makeQgPartition
      val valuesForAgainst = qgpLocal.findResponsesFor("against")
      valuesForAgainst.size mustEqual 1
      val qgUpdated = qgpLocal.addQuizItem(TextValue("against"), TextValue("gegen"))
      qgUpdated.findResponsesFor("against").size mustEqual 2
    }

    "generate false answers similar to a correct answer" in {
      def hasSameEnd = (value1: TextValue, value2: String) => value1.hasSameEnd(value2)

      val falseAnswers = qgPartition.constructWrongChoicesSimilar(
        correctResponses = List("unterhalten"),
        numWrongResponsesRequired = 5,
        correctValue = "unterhalten",
        similarityPredicate = hasSameEnd)

      falseAnswers.contains("unterrichten") mustEqual true
    }

    def pullQuizItem(qgp: QuizGroupPartition, currentPromptNumber: Int):
        (QuizGroupPartition, (String, String)) = {
      val quizItem = qgp.findPresentableQuizItem(currentPromptNumber)
      quizItem.isDefined mustEqual true
      // Each time a quiz item is pulled, a user answer must be set
      val qgpUpdated = qgp.updatedWithUserAnswer(quizItem.get.prompt,
          quizItem.get.correctResponse, true, UserResponses(), new UserResponse(0))
      (qgpUpdated, (quizItem.get.prompt.value, quizItem.get.correctResponse.value))
    }

    "find a presentable quiz item" in {
      pullQuizItem(qgPartition, 0)._2 mustEqual ("against", "wider")
    }

    "remove a quiz pair" in {
      val qgpLocal = makeQgPartition
      val itemToRemove = QuizItem("against", "wider")
      val qgUpdated = qgpLocal.removeQuizItem(itemToRemove)
      qgUpdated.contains("against") mustEqual false
    }

    "add a new quiz item to the front of its queue" in {
      val qgpLocal = makeQgPartition
      val qgUpdated = qgpLocal.addNewQuizItem("to exchange", "tauschen")
      pullQuizItem(qgUpdated, 0)._2 mustEqual ("to exchange", "tauschen")
    }

    "move an existing quiz pair to the front of its queue" in {
      val qgpLocal = makeQgPartition
      val numPromptsBefore = qgpLocal.numPrompts
      val qgUpdated = qgpLocal.addQuizItemToFront(QuizItem("sweeps", "streicht"))
      val numPromptsAfter = qgUpdated.numPrompts
      numPromptsAfter mustEqual numPromptsBefore
      pullQuizItem(qgUpdated, 0)._2 mustEqual ("sweeps", "streicht")
    }

    "move a quiz pair to the front of its queue where only the prompt already exists" in {
      val qgpLocal = makeQgPartition
      val sizeBefore = qgpLocal.size
      val qgUpdated = qgpLocal.addQuizItemToFront(QuizItem("entertain", "bewirten"))
      val sizeAfter = qgUpdated.size
      sizeAfter mustEqual sizeBefore + 1
      pullQuizItem(qgUpdated, 0)._2 mustEqual ("entertain", "bewirten")
    }

    "add more than one new quiz pair to the front of its queue" in {
      val qgpLocal = makeQgPartition
      val qgUpdated1 = qgpLocal.addQuizItemToFront(QuizItem("to exchange", "tauschen"))
      val qgUpdated2 = qgUpdated1.addQuizItemToFront(QuizItem("whole", "ganz"))
      val (qgUnrolled, (keyWord, value)) = pullQuizItem(qgUpdated2, 0)
      (keyWord, value) mustEqual ("whole", "ganz")
      pullQuizItem(qgUnrolled, 1)._2 mustEqual ("to exchange", "tauschen")
    }

    def pullQuizItemAndAnswerCorrectly(qgp: QuizGroupPartition, currentPromptNumber: Int):
        QuizGroupPartition = {
      val quizItem = qgp.findPresentableQuizItem(currentPromptNumber).get
      updateWithUserAnswer(qgp, quizItem, currentPromptNumber)
    }

    def updateWithUserAnswer(qgp: QuizGroupPartition, quizItem: QuizItem,
        currentPromptNumber: Int): QuizGroupPartition = {
      val userAnswer = new UserResponse(currentPromptNumber)
      qgp.updatedWithUserAnswer(quizItem.prompt, quizItem.correctResponse, true,
          UserResponses(), userAnswer)
    }

    "present an item that has been answered before after five prompts" in {
      var qgpLocal = makeQgPartition
      val quizItem0 = qgpLocal.findPresentableQuizItem(0).get
      quizItem0.prompt.value mustEqual "against"
      qgpLocal = updateWithUserAnswer(qgpLocal, quizItem0, 0)

      for (promptNum <- 1 until 5)
        qgpLocal = pullQuizItemAndAnswerCorrectly(qgpLocal, promptNum)

      val quizItem5 = qgpLocal.findPresentableQuizItem(5)
      quizItem5.get.prompt.value mustEqual "against"
    }

  }


}
