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

import scala.util.Try
import com.oranda.libanius.util.Util
import Output._
import com.oranda.libanius.dependencies.AppDependencies
import com.oranda.libanius.model._

object RunQuiz extends App {

  private[this] lazy val l = AppDependencies.logger
  private[this] lazy val dataStore = AppDependencies.dataStore

  runQuiz()

  def runQuiz() {
    output("Running quiz...")
    val availableQuizGroups = dataStore.findAvailableQuizGroups
    val quiz =
      if (!availableQuizGroups.isEmpty)
        Quiz(userQuizGroupSelection(availableQuizGroups.toList))
      else {
        output("No quiz groups found. Defaulting to dummy data.\n")
        Quiz.demoQuiz()
      }
    output("OK, the quiz begins! To quit, type q at any time.\n")
    testUserWithQuizItem(quiz)
  }

  def userQuizGroupSelection(quizGroupHeaders: List[QuizGroupHeader]): Set[QuizGroup] = {
    output("Choose quiz group(s). For more than one, separate with commas, e.g. 1,2,3")
    val choices = ChoiceGroup[QuizGroupHeader](quizGroupHeaders)
    choices.show()

    def selectedQuizGroupHeaders = choices.getSelectionFromInput match {
      case ChosenOptions(selectedChoices) => selectedChoices.asInstanceOf[List[QuizGroupHeader]]
      case _ => List[QuizGroupHeader]()
    }

    selectedQuizGroupHeaders.map(header => dataStore.loadQuizGroupCore(header)).toSet
  }

  def testUserWithQuizItem(quiz: Quiz) {
    showScore(quiz)
    Util.stopwatch(quiz.findPresentableQuizItem, "find quiz items") match {
      case (Some((quizItem, quizGroup))) =>
        val updatedQuiz = updateQuiz(quiz, quizGroup)
        keepShowingQuizItems(updatedQuiz, quizItem, quizGroup)
      case _ =>
        output("No more questions found! Done!")
    }
  }

  def keepShowingQuizItems(quiz: Quiz, quizItem: QuizItemViewWithChoices, quizGroup: QuizGroup) {
    showQuizItemAndProcessResponse(quiz, quizItem) match {
      case (Invalid, updatedQuiz) =>
        output("Invalid input\n")
        keepShowingQuizItems(updatedQuiz, quizItem, quizGroup)
      case (Quit, updatedQuiz) =>
        output("Exiting")
        dataStore.saveQuiz(updatedQuiz, path = AppDependencies.conf.filesDir)
        System.exit(0)
      case (_, updatedQuiz) =>
        testUserWithQuizItem(updatedQuiz)
    }
  }

  def updateQuiz(quiz: Quiz, quizGroup: QuizGroup): Quiz = {
    val updatedQuiz = quiz.addQuizGroup(quizGroup.updatedPromptNumber)
    updatedQuiz.quizGroups.foreach(quizGroup =>
      l.log(quizGroup.promptType + " prompt number is " + quizGroup.currentPromptNumber))
    updatedQuiz
  }

  def showScore(quiz: Quiz) {
    def formatAndPrintScore(scoreStr: String) {
      val scoreStrMaxIndex = scala.math.min(scoreStr.length, 6)
      output("Score: " + scoreStr.substring(0, scoreStrMaxIndex) + "%\n")
    }
    val scoreSoFar = (Util.stopwatch(quiz.scoreSoFar, "scoreSoFar") * 100).toString
    formatAndPrintScore(scoreSoFar)
  }

  def showQuizItemAndProcessResponse(quiz: Quiz, quizItem: QuizItemViewWithChoices):
      (UserConsoleResponse, Quiz) = {
    val wordText = ": what is the " + quizItem.responseType + " for this " + quizItem.promptType + "?"
    val answeredText = " (correctly answered " + quizItem.numCorrectAnswersInARow + " times)"
    val questionText = quizItem.prompt +
        (if (quizItem.quizGroupHeader.quizGroupType == WordMapping) wordText else "") +
        (if (quizItem.numCorrectAnswersInARow > 0) answeredText else "")
    output(questionText + "\n")

    if (quizItem.useMultipleChoice) showChoicesAndProcessResponse(quiz, quizItem)
    else getTextResponseAndProcess(quiz, quizItem)
  }

  def showChoicesAndProcessResponse(quiz: Quiz, quizItem: QuizItemViewWithChoices):
      (UserConsoleResponse, Quiz) = {
    val choices = ChoiceGroup[String](quizItem.allChoices)
    choices.show()

    Try(choices.getSelectionFromInput).recover {
      case e: Exception => Invalid
    }.map(userResponse => processAnswer(quiz, userResponse, quizItem)).get
  }

  def getTextResponseAndProcess(quiz: Quiz, quizItem: QuizItemViewWithChoices):
      (UserConsoleResponse, Quiz) =
    Try(getAnswerFromInput).recover {
      case e: Exception => Invalid
    }.map(userResponse => processAnswer(quiz, userResponse, quizItem)).get

  def processAnswer(quiz: Quiz, userResponse: UserConsoleResponse,
      quizItem: QuizItemViewWithChoices): (UserConsoleResponse, Quiz) = {
    val updatedQuiz = userResponse match {
      case answer: Answer => processUserAnswer(quiz, answer.text, quizItem)
      case _ => quiz
    }
    (userResponse, updatedQuiz)
  }

  def processUserAnswer(quiz: Quiz, userAnswerTxt: String,
      quizItem: QuizItemViewWithChoices): Quiz = {
    val correctAnswer: String = quizItem.response.text
    val isCorrect = userAnswerTxt == correctAnswer
    if (isCorrect) output("\nCorrect!\n") else output("\nWrong! It's " + correctAnswer + "\n")

    Util.stopwatch(quiz.updateWithUserAnswer(isCorrect, quizItem), "updateQuiz")
  }

  def getAnswerFromInput: UserConsoleResponse =
    readLine match {
      case "q" => Quit
      case "quit" => Quit
      case input: String => TextAnswer(input)
    }
}

