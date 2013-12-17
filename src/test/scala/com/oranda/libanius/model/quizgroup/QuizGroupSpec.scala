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
import com.oranda.libanius.model.quizitem.{QuizItem, QuizItemViewWithChoices, TextValue}

import java.lang.StringBuilder
import com.oranda.libanius.model.{UserResponse, UserResponses}

class QuizGroupSpec extends Specification with AppDependencyAccess {

  "a quiz group" should {

    val qgCustomFormat =
      "quizGroup type=\"WordMapping\" promptType=\"English word\" responseType=\"German word\" mainSeparator=\"|\" currentPromptNumber=\"10\" isActive=\"true\"\n" +
        "en route|unterwegs|\n" +
        "full|satt|\n" +
        "full|voll|\n" +
        "interrupted|unterbrochen|\n" +
        "contract|Vertrag|\n" +
        "rides|reitet|\n" +
        "on|auf|\n" +
        "sweeps|streicht|\n" +
        "entertain|unterhalten|8;2\n" +
        "winner|Siegerin|5;0\n" +
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

    def makeSimpleQuizGroup = QuizGroup(List(makeQgPartition0), QuizGroupUserData(true, 0))

    def makeQgWithHeader: QuizGroupWithHeader = makeQgwh(makeQuizGroup)
    def makeSimpleQgWithHeader: QuizGroupWithHeader = makeQgwh(makeSimpleQuizGroup)

    def makeQgwh(quizGroup: QuizGroup): QuizGroupWithHeader = {
      val header = QuizGroupHeader(WordMapping, "English word", "German word", "|")
      QuizGroupWithHeader(header, quizGroup)
    }

    // defaults for read-only
    val qgWithHeader = makeQgWithHeader

    def updateWithUserAnswer(qg: QuizGroup, quizItem: QuizItemViewWithChoices): QuizGroup = {
      val userAnswer = new UserResponse(qg.currentPromptNumber)
      qg.updatedWithUserResponse(quizItem.prompt, quizItem.correctResponse, true, UserResponses(),
        userAnswer).updatedPromptNumber
    }

    def pullQuizItemAndAnswerCorrectly(qgwh: QuizGroupWithHeader): QuizGroupWithHeader = {
      val quizItem = qgwh.findPresentableQuizItem.get
      l.log("quizItem: " + quizItem)
      QuizGroupWithHeader(qgwh.header, updateWithUserAnswer(qgwh.quizGroup, quizItem))
    }

    "be parseable from custom format" in {
      val qgWithHeaderLocal = QuizGroupWithHeader.fromCustomFormat(qgCustomFormat)
      qgWithHeaderLocal.currentPromptNumber mustEqual 10
      qgWithHeaderLocal.promptType mustEqual "English word"
      qgWithHeaderLocal.responseType mustEqual "German word"

      qgWithHeaderLocal.partitions.size mustEqual 5
      qgWithHeaderLocal.partitions(0).size mustEqual 8
      qgWithHeaderLocal.partitions(1).size mustEqual 2
      qgWithHeaderLocal.partitions(2).size mustEqual 2
    }

    "be serializable to custom format" in {
      val customFormat = qgWithHeader.quizGroup.toCustomFormat(new StringBuilder(),
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
      val quizGroupLocal = makeQuizGroup
      val qgUpdated = quizGroupLocal.updatedPromptNumber
      qgUpdated.currentPromptNumber mustEqual 11
    }

    "accept the addition of a new word-mapping" in {
      val quizGroupLocal = makeQuizGroup
      quizGroupLocal.contains("good") mustEqual false
      val qgUpdated = quizGroupLocal.addNewQuizItem("good", "gut")
      qgUpdated.contains("good") mustEqual true
    }

    "update partitions on a user response" in {
      val qgwhLocal = makeSimpleQgWithHeader
      qgwhLocal.quizGroup.partitions(1).size mustEqual 0
      val quizItem = qgwhLocal.quizGroup.findPresentableQuizItem
      val qgUpdated = qgwhLocal.quizGroup.updatedWithUserResponse(quizItem.get.prompt,
          quizItem.get.correctResponse, true, UserResponses(),
          new UserResponse(qgwhLocal.currentPromptNumber))
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
      val qgLocal = makeQuizGroup
      val valuesForAgainst = qgLocal.findResponsesFor("against")
      valuesForAgainst.size mustEqual 1
      val qgUpdated = qgLocal.addNewQuizItem("against", "gegen")
      qgUpdated.findResponsesFor("against").size mustEqual 2
    }

    "make false answers" in {
      val quizItemCorrect: QuizItem = qgWithHeader.quizGroup.quizItems.find(
          _.prompt.value == "entertain").get

      val falseAnswers = qgWithHeader.quizGroup.constructWrongChoices(
          quizItemCorrect: QuizItem, numCorrectResponsesSoFar = 1,
          numWrongChoicesRequired = 2)

      l.log("falseAnswers: " + falseAnswers)
      falseAnswers.contains("unterbrochen") mustEqual true
    }

    "remove a quiz pair" in {
      val qgLocal = makeQuizGroup
      val itemToRemove = QuizItem("against", "wider")
      val qgUpdated = qgLocal.removeQuizItem(itemToRemove)
      qgUpdated.contains("against") mustEqual false
    }

    "add a new quiz item to the front of its queue" in {
      val qgLocal = makeSimpleQuizGroup
      val qgUpdated = qgLocal.addNewQuizItem("to exchange", "tauschen")
      qgUpdated.findPresentableQuizItem mustEqual Some(QuizItem("to exchange", "tauschen"))
    }

    "move an existing quiz pair to the front of its queue" in {
      val qgwhLocal = makeQgWithHeader
      val numPromptsBefore = qgwhLocal.numPrompts
      val qgUpdated = qgwhLocal.addNewQuizItem("sweeps", "streicht")
      val numPromptsAfter = qgUpdated.numPrompts
      numPromptsAfter mustEqual numPromptsBefore
      val qgwhUpdated = QuizGroupWithHeader(qgwhLocal.header, qgUpdated)
      qgwhUpdated.quizGroup.partitions(0).findPresentableQuizItem(
          qgwhUpdated.currentPromptNumber) mustEqual Some(QuizItem("sweeps", "streicht"))
    }

    "add a quiz pair where only the prompt already exists" in {
      val qgwhLocal = makeQgWithHeader
      val sizeBefore = qgwhLocal.size
      val qgUpdated = qgwhLocal.addNewQuizItem("on", "zu")
      val sizeAfter = qgUpdated.size
      sizeAfter mustEqual sizeBefore + 1
      val qgwhUpdated = QuizGroupWithHeader(qgwhLocal.header, qgUpdated)
      qgwhUpdated.quizGroup.partitions(0).findPresentableQuizItem(
          qgwhUpdated.currentPromptNumber) mustEqual Some(QuizItem("on", "zu"))
    }


    "add more than one new quiz pair" in {
      val qgwhLocal = makeSimpleQgWithHeader
      val qgUpdated1 = qgwhLocal.addNewQuizItem("to exchange", "tauschen")
      val qgUpdated2 = qgUpdated1.addNewQuizItem("whole", "ganz")
      val qgwhUpdated2 = QuizGroupWithHeader(qgwhLocal.header, qgUpdated2)
      val (qgUnrolled, (keyWord, value)) = pullQuizItem(qgwhUpdated2)
      (keyWord, value) mustEqual ("whole", "ganz")
      val qgwhUnrolled = QuizGroupWithHeader(qgwhLocal.header, qgUnrolled)
      pullQuizItem(qgwhUnrolled)._2 mustEqual ("to exchange", "tauschen")
    }
  }
}
