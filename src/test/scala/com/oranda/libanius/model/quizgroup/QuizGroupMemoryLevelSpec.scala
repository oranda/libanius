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

import com.oranda.libanius.model.{MemoryLevels, UserResponse, UserResponses}
import java.lang.StringBuilder

class QuizGroupMemoryLevelSpec extends Specification with AppDependencyAccess {

  "a quiz group partition" should {

    implicit val ml = MemoryLevels()

    /*
     * Construct a quiz group partition.
     */
    def makeQgPartition: QuizGroupMemoryLevel = QuizGroupMemoryLevel(List(
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

    def makeQgPartitionSimple: QuizGroupMemoryLevel = QuizGroupMemoryLevel(List(
        QuizItem("against", "wider"),
        QuizItem("entertain", "unterhalten")).toStream)


    // defaults for read-only
    val qgPartition = makeQgPartition
    val qgPartitionSimple = makeQgPartitionSimple

    val qgPartitionSimpleCustomFormat =
        "against|wider|\n" +
        "entertain|unterhalten|\n"


    "be parseable from custom format" in {
      val qgp = QuizGroupMemoryLevel.fromCustomFormat(
          qgPartitionSimpleCustomFormat, "|")
      qgp.numQuizItems mustEqual 2
    }

    "be serializable to custom format" in {
      val customFormat = qgPartitionSimple.toCustomFormat(new StringBuilder(), "|", 0)
      customFormat.toString mustEqual
          "#quizGroupPartition numCorrectResponsesInARow=\"0\"\n" + qgPartitionSimpleCustomFormat
    }

    "find values for a prompt" in {
      qgPartition.findResponsesFor("on") mustEqual List("auf")
    }

    "accept the addition of a new word-mapping" in {
      qgPartition.contains("good") mustEqual false
      val qgUpdated = qgPartition.addNewQuizItem("good", "gut")
      qgUpdated.contains("good") mustEqual true
    }

    "accept new values for an existing word-mapping" in {
      val valuesForAgainst = qgPartition.findResponsesFor("against")
      valuesForAgainst.size mustEqual 1
      val qgUpdated = qgPartition.addQuizItem(TextValue("against"), TextValue("gegen"))
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

    def pullQuizItem(qgp: QuizGroupMemoryLevel, currentPromptNumber: Int):
        (QuizGroupMemoryLevel, (String, String)) = {
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
      val itemToRemove = QuizItem("against", "wider")
      val qgUpdated = qgPartition.removeQuizItem(itemToRemove)
      qgUpdated.contains("against") mustEqual false
    }

    "add a new quiz item to the front of its queue" in {
      val qgUpdated = qgPartition.addNewQuizItem("to exchange", "tauschen")
      pullQuizItem(qgUpdated, 0)._2 mustEqual ("to exchange", "tauschen")
    }

    "move an existing quiz pair to the front of its queue" in {
      val numPromptsBefore = qgPartition.numPrompts
      val qgUpdated = qgPartition.addQuizItemToFront(QuizItem("sweeps", "streicht"))
      val numPromptsAfter = qgUpdated.numPrompts
      numPromptsAfter mustEqual numPromptsBefore
      pullQuizItem(qgUpdated, 0)._2 mustEqual ("sweeps", "streicht")
    }

    "move a quiz pair to the front of its queue where only the prompt already exists" in {
      val sizeBefore = qgPartition.size
      val qgUpdated = qgPartition.addQuizItemToFront(QuizItem("entertain", "bewirten"))
      val sizeAfter = qgUpdated.size
      sizeAfter mustEqual sizeBefore + 1
      pullQuizItem(qgUpdated, 0)._2 mustEqual ("entertain", "bewirten")
    }

    "add more than one new quiz pair to the front of its queue" in {
      val qgUpdated1 = qgPartition.addQuizItemToFront(QuizItem("to exchange", "tauschen"))
      val qgUpdated2 = qgUpdated1.addQuizItemToFront(QuizItem("whole", "ganz"))
      val (qgUnrolled, (keyWord, value)) = pullQuizItem(qgUpdated2, 0)
      (keyWord, value) mustEqual ("whole", "ganz")
      pullQuizItem(qgUnrolled, 1)._2 mustEqual ("to exchange", "tauschen")
    }

    def pullQuizItemAndAnswerCorrectly(qgp: QuizGroupMemoryLevel, currentPromptNumber: Int):
        QuizGroupMemoryLevel = {
      val quizItem = qgp.findPresentableQuizItem(currentPromptNumber).get
      updateWithUserAnswer(qgp, quizItem, currentPromptNumber)
    }

    def updateWithUserAnswer(qgp: QuizGroupMemoryLevel, quizItem: QuizItem,
        currentPromptNumber: Int): QuizGroupMemoryLevel = {
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
