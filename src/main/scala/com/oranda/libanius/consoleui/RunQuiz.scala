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

package com.oranda.libanius.consoleui

import com.oranda.libanius.io.StandardIO
import com.oranda.libanius.model.wordmapping._
import scala.util.Try
import com.oranda.libanius.util.Util
import Output._
import com.oranda.libanius.model.wordmapping.QuizItemViewWithOptions
import com.oranda.libanius.dependencies.{Conf, AppDependencies}

object RunQuiz extends App {

  private[this] lazy val l = AppDependencies.logger
  private[this] lazy val dataStore = AppDependencies.dataStore

  runQuiz()

  def runQuiz() {
    output("Running quiz...")
    val availableQuizGroups = dataStore.findAvailableWmgs
    val quiz =
      if (!availableQuizGroups.isEmpty)
        QuizOfWordMappings(userQuizGroupSelection(availableQuizGroups.toList))
      else {
        output("No quiz groups found. Defaulting to dummy data.\n")
        QuizOfWordMappings.demoQuiz()
      }
    output("OK, the quiz begins! To quit, type q at any time.\n")
    testUserWithQuizItem(quiz)
  }

  def userQuizGroupSelection(quizGroupHeaders: List[QuizGroupHeader]): Set[WordMappingGroup] = {
    output("Choose quiz group(s). For more than one, separate with commas, e.g. 1,2,3")
    val options = OptionGroup[QuizGroupHeader](quizGroupHeaders)
    options.show()

    def selectedQuizGroupHeaders = options.getSelectionFromInput match {
      case Options(selectedOptions) => selectedOptions.asInstanceOf[List[QuizGroupHeader]]
      case _ => List[QuizGroupHeader]()
    }

    selectedQuizGroupHeaders.map(header => dataStore.loadWmgCore(header)).toSet
  }

  def testUserWithQuizItem(quiz: QuizOfWordMappings) {
    showScore(quiz)
    Util.stopwatch(quiz.findQuizItem, "find quiz items") match {
      case (Some((quizItem, wmg)), failedWmgs) =>
        val updatedQuiz = updateQuiz(quiz, wmg, failedWmgs)
        keepShowingQuizItems(updatedQuiz, quizItem, wmg, failedWmgs)
      case _ =>
        output("No more questions found! Done!")
    }
  }

  def keepShowingQuizItems(quiz: QuizOfWordMappings, quizItem: QuizItemViewWithOptions,
      wmg: WordMappingGroup, failedWmgs: List[WordMappingGroup]) {
    showQuizItemAndProcessResponse(quiz, quizItem) match {
      case (Invalid, updatedQuiz) =>
        output("Invalid input\n")
        keepShowingQuizItems(updatedQuiz, quizItem, wmg, failedWmgs)
      case (Quit, updatedQuiz) =>
        output("Exiting")
        dataStore.saveQuiz(updatedQuiz, path = AppDependencies.conf.filesDir)
        System.exit(0)
      case (_, updatedQuiz) =>
        testUserWithQuizItem(updatedQuiz)
    }
  }

  def updateQuiz(quiz: QuizOfWordMappings, wmg: WordMappingGroup,
      failedWmgs: List[WordMappingGroup]): QuizOfWordMappings = {
    val updatedQuiz = quiz.addWordMappingGroup(wmg.updatedPromptNumber).
        updateRangeForFailedWmgs(failedWmgs)
    updatedQuiz.wordMappingGroups.foreach(wmg => l.log(wmg.keyType + " prompt number is " +
        wmg.currentPromptNumber + ", range is " + wmg.currentSearchRange.start))
    updatedQuiz
  }

  def showScore(quiz: QuizOfWordMappings) {
    def formatAndPrintScore(scoreStr: String) {
      val scoreStrMaxIndex = scala.math.min(scoreStr.length, 6)
      output("Score: " + scoreStr.substring(0, scoreStrMaxIndex) + "%\n")
    }
    val scoreSoFar = (Util.stopwatch(quiz.scoreSoFar, "scoreSoFar") * 100).toString
    formatAndPrintScore(scoreSoFar)
  }

  def showQuizItemAndProcessResponse(quiz: QuizOfWordMappings, quizItem: QuizItemViewWithOptions):
      (UserResponse, QuizOfWordMappings) = {

    val questionText = quizItem.keyWord + ": what is the " + quizItem.valueType +
        " for this " + quizItem.keyType + "?" +
        (if (quizItem.numCorrectAnswersInARow > 0)
           " (correctly answered " + quizItem.numCorrectAnswersInARow + " times)"
         else "")
    output(questionText + "\n")

    val options = OptionGroup[String](quizItem.allOptions)
    options.show()

    Try(options.getSelectionFromInput).recover {
      case e: Exception => Invalid
    }.map(userResponse => processAnswerFromOptions(quiz, userResponse, quizItem, options)).get
  }

  def processAnswerFromOptions(quiz: QuizOfWordMappings, userResponse: UserResponse,
      quizItem: QuizItemViewWithOptions, options: OptionGroup[String]):
      (UserResponse, QuizOfWordMappings) = {
    val updatedQuiz = userResponse match {
      case Options(options) => processUserAnswer(quiz, options.iterator.next.toString, quizItem)
      case _ => quiz
    }
    (userResponse, updatedQuiz)
  }

  def processUserAnswer(quiz: QuizOfWordMappings, userAnswerTxt: String,
      quizItem: QuizItemViewWithOptions): QuizOfWordMappings = {
    val correctAnswer = quizItem.wordMappingValue.value
    val isCorrect = userAnswerTxt == correctAnswer
    if (isCorrect) output("\nCorrect!\n")
    else output("\nWrong!\n")

    Util.stopwatch(quiz.updateWithUserAnswer(isCorrect, quizItem), "updateQuiz")
  }
}

