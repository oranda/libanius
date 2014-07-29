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
import com.oranda.libanius.model.quizitem.QuizItem

import com.oranda.libanius.util.Util

import com.oranda.libanius.model.TestData._
import com.oranda.libanius.model._

import CustomFormat._
import CustomFormatForModelComponents._

class QuizGroupSpec extends Specification with AppDependencyAccess {

  "a quiz group" should {

    "find values for a prompt" in {
      qgWithHeader.findResponsesFor("on") mustEqual List("auf")
    }

    def pullQuizItem(qgwh: QuizGroupWithHeader): (QuizGroup, (String, String)) = {
      val qiView = qgwh.findPresentableQuizItem
      qiView.isDefined mustEqual true
      val quizItem = qiView.get.quizItem
      // Each time a quiz item is pulled, a user answer must be set
      val qgUpdated = updatedWithUserResponse(qgwh.quizGroup, quizItem)
      (qgUpdated, (quizItem.prompt.value, quizItem.correctResponse.value))
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

    "update memory levels on a user response" in {
      qgwhSimple.quizGroup.levels(1).size mustEqual 0
      val quizItem = qgwhSimple.quizGroup.findPresentableQuizItem
      val qgUpdated = updatedWithUserResponse(qgwhSimple.quizGroup, quizItem.get)
      qgUpdated.levels(1).size mustEqual 1
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
          updatedWithUserResponse(qgwhLocal, quizItem0.quizItem))
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

    "construct wrong choices" in {
      val quizItemCorrect: QuizItem = qgWithHeader.quizGroup.quizItems.find(
          _.prompt.value == "entertain").get

      val (falseAnswers, timeTaken) = Util.stopwatch(qgWithHeader.quizGroup.constructWrongChoices(
          quizItemCorrect, numCorrectResponsesSoFar = 1, numWrongChoicesRequired = 2))

      falseAnswers.contains("unterbrochen") mustEqual true
      timeTaken must be lessThan 100
    }

    /**
     * This reproduces a (former) bug, where only one wrong choice was being produced.
     */
    "construct a full set of wrong choices" in {

      val demoGroupHeader = "#quizGroup type=\"WordMapping\" promptType=\"German word\" responseType=\"English word\" currentPromptNumber=\"21\" isActive=\"true\"\n"
      val demoGroupText = demoGroupHeader +
          "#quizGroupPartition numCorrectResponsesInARow=\"0\" repetitionInterval=\"0\"\n" +
          "entertain|unterhalten'\n" +
          "#quizGroupPartition numCorrectResponsesInARow=\"1\" repetitionInterval=\"5\"\n" +
          "against|wider'13\n" +
          "#quizGroupPartition numCorrectResponsesInARow=\"2\" repetitionInterval=\"15\"\n" +
          "treaty|Vertrag'11,5\n" +
          "contract|Vertrag'9,3\n" +
          "en route|unterwegs'7,1\n"

      val demoGroup: QuizGroup = from[QuizGroup, FromParamsWithSeparator](
          demoGroupText, FromParamsWithSeparator("|"))
      val quizItemCorrect: QuizItem = demoGroup.quizItems.find(_.prompt.value == "treaty").get

      val (falseAnswers, _) = Util.stopwatch(demoGroup.constructWrongChoices(
          quizItemCorrect, numCorrectResponsesSoFar = 2, numWrongChoicesRequired = 2))

      falseAnswers.size mustEqual 2
    }

    "remove a quiz pair" in {
      val itemToRemove = QuizItem("against", "wider")
      val qgUpdated = quizGroup.removeQuizItem(itemToRemove)
      qgUpdated.contains("against") mustEqual false
    }

    "add a new quiz item to the front of its queue" in {
      val qgUpdated = quizGroupSimple.addNewQuizItem("to exchange", "tauschen")
      qgUpdated.findPresentableQuizItem mustEqual Some(
          QuizItem("to exchange", "tauschen"))
    }

    "move an existing quiz pair to the front of its queue" in {
      val numPromptsBefore = qgwh.numPrompts
      val qgUpdated = qgwh.addNewQuizItem("sweeps", "streicht")
      val numPromptsAfter = qgUpdated.numPrompts
      numPromptsAfter mustEqual numPromptsBefore
      val qgwhUpdated = QuizGroupWithHeader(qgwh.header, qgUpdated)
      qgwhUpdated.quizGroup.levels(0).findPresentableQuizItem(
          qgwhUpdated.currentPromptNumber) mustEqual Some(QuizItem("sweeps", "streicht"))
    }

    "add a quiz pair where only the prompt already exists" in {
      val sizeBefore = qgwh.size
      val qgUpdated = qgwh.addNewQuizItem("on", "zu")
      val sizeAfter = qgUpdated.size
      sizeAfter mustEqual sizeBefore + 1
      val qgwhUpdated = QuizGroupWithHeader(qgwh.header, qgUpdated)
      qgwhUpdated.quizGroup.levels(0).findPresentableQuizItem(
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

    val memLevel2 = QuizGroupMemoryLevel(repetitionInterval = 15, totalResponses = 2,
        numCorrectResponses = 1)
    val qg = QuizGroup(Map(2 -> memLevel2))

    "be updated for a correct answer" in {
      val updatedQg = qg.incrementResponsesCorrect(2)
      updatedQg.totalResponses(2) mustEqual 3
      updatedQg.numCorrectResponses(2) mustEqual 2
    }

    "be updated for a incorrect answer" in {
      val updatedQg = qg.incrementResponsesIncorrect(2)
      updatedQg.totalResponses(2) mustEqual 3
      updatedQg.numCorrectResponses(2) mustEqual 1
    }

    "throw an exception on an attempt to get information from an unknown memory level" in {
      qg.totalResponses(9) must throwA[IndexOutOfBoundsException]
    }

    "have a count reset for a level" in {
      val memLevel2 = QuizGroupMemoryLevel(repetitionInterval = 15, totalResponses = 10,
        numCorrectResponses = 1)
      val qg = QuizGroup(Map(2 -> memLevel2))
      val updatedQg = qg.incrementResponsesCorrect(2)
      updatedQg.totalResponses(2) mustEqual 1
      updatedQg.numCorrectResponses(2) mustEqual 1
    }
  }
}
