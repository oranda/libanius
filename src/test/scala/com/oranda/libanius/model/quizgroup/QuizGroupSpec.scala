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
import com.oranda.libanius.model.quizitem.{QuizItem, QuizItemViewWithChoices}

import com.oranda.libanius.model.{Criteria, UserResponse, UserResponses}
import com.oranda.libanius.util.Util

class QuizGroupSpec extends Specification with AppDependencyAccess {

  "a quiz group" should {

    val qgCustomFormat =
      "#quizGroup type=\"WordMapping\" promptType=\"English word\" responseType=\"German word\" mainSeparator=\"|\" currentPromptNumber=\"10\" isActive=\"true\"\n" +
        "#quizGroupPartition numCorrectResponsesInARow=\"0\"\n" +
        "en route|unterwegs|\n" +
        "full|satt|\n" +
        "full|voll|\n" +
        "interrupted|unterbrochen|\n" +
        "contract|Vertrag|\n" +
        "rides|reitet|\n" +
        "on|auf|\n" +
        "sweeps|streicht|\n" +
        "#quizGroupPartition numCorrectResponsesInARow=\"1\"\n" +
        "entertain|unterhalten|8;2\n" +
        "winner|Siegerin|5;0\n" +
        "#quizGroupPartition numCorrectResponsesInARow=\"2\"\n" +
        "against|wider|9,7;6\n" +
        "teach|unterrichten|4,3;1\n"

    def makeQgPartition0: QuizGroupPartition = QuizGroupPartition(List(
      QuizItem("en route", "unterwegs"),
      QuizItem("full", "satt"),
      QuizItem("full", "voll"),
      QuizItem("interrupted", "unterbrochen"),
      QuizItem("contract", "Vertrag"),
      QuizItem("rides", "reitet"),
      QuizItem("on", "auf"),
      QuizItem("sweeps", "streicht")).toStream)

    def makeQgPartition1: QuizGroupPartition = QuizGroupPartition(List(
      QuizItem("entertain", "unterhalten", List(8), List(2)),
      QuizItem("winner", "Siegerin", List(5), List(0))).toStream)

    def makeQgPartition2: QuizGroupPartition = QuizGroupPartition(List(
      QuizItem("against", "wider", List(9, 7), List(6)),
      QuizItem("teach", "unterrichten", List(4, 3), List(1))).toStream)

    def makeQuizGroup = QuizGroup(
        List(makeQgPartition0, makeQgPartition1, makeQgPartition2), QuizGroupUserData(true, 10))
    val quizGroup = makeQuizGroup

    def makeSimpleQuizGroup = QuizGroup(List(makeQgPartition0), QuizGroupUserData(true, 0))
    val quizGroupSimple = makeSimpleQuizGroup



    def makeQgwh(quizGroup: QuizGroup): QuizGroupWithHeader = {
      val header = QuizGroupHeader(WordMapping, "English word", "German word", "|")
      QuizGroupWithHeader(header, quizGroup)
    }
    val qgwh: QuizGroupWithHeader = makeQgwh(quizGroup)

    def makeQgWithHeader: QuizGroupWithHeader = makeQgwh(makeQuizGroup)
    def makeSimpleQgWithHeader: QuizGroupWithHeader = makeQgwh(makeSimpleQuizGroup)

    val qgwhSimple: QuizGroupWithHeader = makeSimpleQgWithHeader

    // defaults for read-only
    val qgWithHeader = makeQgWithHeader

    def updateWithUserAnswer(qg: QuizGroup, quizItem: QuizItemViewWithChoices): QuizGroup = {
      val userAnswer = new UserResponse(qg.currentPromptNumber)
      qg.updatedWithUserResponse(quizItem.prompt, quizItem.correctResponse, true, UserResponses(),
        userAnswer).updatedPromptNumber
    }

    def pullQuizItemAndAnswerCorrectly(qgwh: QuizGroupWithHeader): QuizGroupWithHeader = {
      val quizItem = qgwh.findPresentableQuizItem.get
      QuizGroupWithHeader(qgwh.header, updateWithUserAnswer(qgwh.quizGroup, quizItem))
    }

    "be parseable from custom format" in {
      qgWithHeader.currentPromptNumber mustEqual 10
      qgWithHeader.promptType mustEqual "English word"
      qgWithHeader.responseType mustEqual "German word"

      qgWithHeader.partitions.size mustEqual Criteria.numCorrectResponsesRequired + 1
      qgWithHeader.partitions(0).size mustEqual 8
      qgWithHeader.partitions(1).size mustEqual 2
      qgWithHeader.partitions(2).size mustEqual 2
    }

    "be serializable to custom format" in {
      val customFormat = qgWithHeader.quizGroup.toCustomFormat(new java.lang.StringBuilder(),
          qgWithHeader.header)
      customFormat.toString mustEqual qgCustomFormat
    }

    "find values for a prompt" in {
      qgWithHeader.findResponsesFor("on") mustEqual List("auf")
    }

    def pullQuizItem(qgwh: QuizGroupWithHeader): (QuizGroup, (String, String)) = {
      val quizItem = qgwh.findPresentableQuizItem
      quizItem.isDefined mustEqual true
      // Each time a quiz item is pulled, a user answer must be set
      val qgUpdated = qgwh.quizGroup.updatedWithUserResponse(quizItem.get.prompt,
          quizItem.get.correctResponse, true, UserResponses(),
          new UserResponse(qgwh.currentPromptNumber))
      (qgUpdated, (quizItem.get.prompt.value, quizItem.get.correctResponse.value))
    }

    "find a presentable quiz item" in {
      pullQuizItem(qgWithHeader)._2 mustEqual ("winner", "Siegerin")
    }

    "accept an updated prompt number" in {
      val qgUpdated = quizGroup.updatedPromptNumber
      qgUpdated.currentPromptNumber mustEqual 11
    }

    "accept the addition of a new word-mapping" in {
      quizGroup.contains("good") mustEqual false
      val qgUpdated = quizGroup.addNewQuizItem("good", "gut")
      qgUpdated.contains("good") mustEqual true
    }

    "update partitions on a user response" in {
      qgwhSimple.quizGroup.partitions(1).size mustEqual 0
      val quizItem = qgwhSimple.quizGroup.findPresentableQuizItem
      val qgUpdated = qgwhSimple.quizGroup.updatedWithUserResponse(quizItem.get.prompt,
          quizItem.get.correctResponse, true, UserResponses(),
          new UserResponse(qgwhSimple.currentPromptNumber))
      qgUpdated.partitions(1).size mustEqual 1
    }

    /*
     * This assumes the following Criteria in UserResponses:
     * Criteria(numCorrectResponsesInARowDesired = 1, diffInPromptNumMinimum = 5),
     */
    "present an item that has been answered before after five prompts" in {
      var qgwhLocal = makeSimpleQgWithHeader
      val quizItem0 = qgwhLocal.findPresentableQuizItem.get
      quizItem0.prompt.value mustEqual "en route" // "against"
      qgwhLocal = QuizGroupWithHeader(qgwhLocal.header,
          updateWithUserAnswer(qgwhLocal, quizItem0))

      for (promptNum <- 1 until 5)
        qgwhLocal = pullQuizItemAndAnswerCorrectly(qgwhLocal)

      val quizItem5 = qgwhLocal.findPresentableQuizItem
      quizItem5.get.prompt.value mustEqual "en route" // "against"
    }

    "accept new values for an existing word-mapping" in {
      val valuesForAgainst = quizGroup.findResponsesFor("against")
      valuesForAgainst.size mustEqual 1
      val qgUpdated = quizGroup.addNewQuizItem("against", "gegen")
      qgUpdated.findResponsesFor("against").size mustEqual 2
    }

    "make false answers" in {
      val quizItemCorrect: QuizItem = qgWithHeader.quizGroup.quizItems.find(
          _.prompt.value == "entertain").get

      val (falseAnswers, timeTaken) = Util.stopwatch(qgWithHeader.quizGroup.constructWrongChoices(
          quizItemCorrect: QuizItem, numCorrectResponsesSoFar = 1,
          numWrongChoicesRequired = 2))

      l.log("falseAnswers: " + falseAnswers)
      falseAnswers.contains("unterbrochen") mustEqual true
      timeTaken must be lessThan 100
    }

    "remove a quiz pair" in {
      val itemToRemove = QuizItem("against", "wider")
      val qgUpdated = quizGroup.removeQuizItem(itemToRemove)
      qgUpdated.contains("against") mustEqual false
    }

    "add a new quiz item to the front of its queue" in {
      val qgUpdated = quizGroupSimple.addNewQuizItem("to exchange", "tauschen")
      qgUpdated.findPresentableQuizItem mustEqual Some(QuizItem("to exchange", "tauschen"))
    }

    "move an existing quiz pair to the front of its queue" in {
      val numPromptsBefore = qgwh.numPrompts
      val qgUpdated = qgwh.addNewQuizItem("sweeps", "streicht")
      val numPromptsAfter = qgUpdated.numPrompts
      numPromptsAfter mustEqual numPromptsBefore
      val qgwhUpdated = QuizGroupWithHeader(qgwh.header, qgUpdated)
      qgwhUpdated.quizGroup.partitions(0).findPresentableQuizItem(
          qgwhUpdated.currentPromptNumber) mustEqual Some(QuizItem("sweeps", "streicht"))
    }

    "add a quiz pair where only the prompt already exists" in {
      val sizeBefore = qgwh.size
      val qgUpdated = qgwh.addNewQuizItem("on", "zu")
      val sizeAfter = qgUpdated.size
      sizeAfter mustEqual sizeBefore + 1
      val qgwhUpdated = QuizGroupWithHeader(qgwh.header, qgUpdated)
      qgwhUpdated.quizGroup.partitions(0).findPresentableQuizItem(
          qgwhUpdated.currentPromptNumber) mustEqual Some(QuizItem("on", "zu"))
    }


    "add more than one new quiz pair" in {
      val qgwhSimple = makeSimpleQgWithHeader
      val qgUpdated1 = qgwhSimple.addNewQuizItem("to exchange", "tauschen")
      val qgUpdated2 = qgUpdated1.addNewQuizItem("whole", "ganz")
      val qgwhUpdated2 = QuizGroupWithHeader(qgwhSimple.header, qgUpdated2)
      val (qgUnrolled, (keyWord, value)) = pullQuizItem(qgwhUpdated2)
      (keyWord, value) mustEqual ("whole", "ganz")
      val qgwhUnrolled = QuizGroupWithHeader(qgwhSimple.header, qgUnrolled)
      pullQuizItem(qgwhUnrolled)._2 mustEqual ("to exchange", "tauschen")
    }
  }
}
