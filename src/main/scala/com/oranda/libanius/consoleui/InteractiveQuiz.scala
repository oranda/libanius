/*
 * Libanius
 * Copyright (C) 2012-2016 James McCabe <james@oranda.com>
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

package com.oranda.libanius.consoleui

import scala.util.Try
import com.oranda.libanius.util.{Util, StringUtil}
import Output._
import ConsoleUtil._
import com.oranda.libanius.model._
import com.oranda.libanius.dependencies._
import com.oranda.libanius.model.quizitem.QuizItemViewWithChoices
import com.oranda.libanius.model.quizgroup.{WordMapping, QuizGroupHeader, QuizGroup}

import com.oranda.libanius.model.action._
import QuizItemSource._
import modelComponentsAsQuizItemSources._

import scalaz._

trait InteractiveQuiz extends App with AppDependencyAccess {

  def userQuizGroupSelection(
      quizGroupHeaders: List[QuizGroupHeader]): Map[QuizGroupHeader, QuizGroup] = {
    output("Choose quiz group(s). For more than one, separate with commas, e.g. 1,2,3")
    val choices = ChoiceGroup[QuizGroupHeader](quizGroupHeaders)
    choices.show()

    def selectedQuizGroupHeaders = choices.getSelectionFromInput match {
      case ChosenOptions(selectedChoices) => selectedChoices.asInstanceOf[List[QuizGroupHeader]]
      case _ => List[QuizGroupHeader]()
    }

    selectedQuizGroupHeaders.map(header => (header, dataStore.loadQuizGroupCore(header))).toMap
  }

  def testUserWithQuizItem(quiz: Quiz): Unit = {
    showScore(quiz)
    Util.stopwatch(produceQuizItem(quiz, NoParams()), "find quiz items") match {
      case Some(quizItem) => keepShowingQuizItems(quiz, quizItem)
      case _ => output("No more questions found! Done!")
    }
  }

  def keepShowingQuizItems(quiz: Quiz, quizItem: QuizItemViewWithChoices): Unit = {
    val (updatedQuiz, response) = showQuizItemAndProcessResponse(quizItem).run(quiz)
    response match {
      case Invalid =>
        output("Invalid input\n")
        keepShowingQuizItems(updatedQuiz, quizItem)
      case Quit =>
        output("Exiting... .")
        saveQuiz(updatedQuiz)
      case _ =>
        testUserWithQuizItem(updatedQuiz)
    }
  }

  def saveQuiz(quiz: Quiz): Unit = {
    dataStore.saveQuiz(quiz, path = conf.filesDir)
  }

  def showScore(quiz: Quiz): Unit = {
    val score: BigDecimal = Util.stopwatch(quiz.scoreSoFar, "scoreSoFar")
    val formattedScore = StringUtil.formatScore(score)
    output(s"Score: $formattedScore")
  }

  def showQuizItemAndProcessResponse(
      quizItem: QuizItemViewWithChoices): State[Quiz, UserConsoleResponse] = {
    val wordText = s": what is the ${quizItem.responseType} for this ${quizItem.promptType}?"
    val wordTextToShow =
      if (quizItem.quizGroupHeader.quizGroupType == WordMapping) wordText else ""
    val answeredText = s" (correctly answered ${quizItem.numCorrectResponsesInARow} times)"
    val answeredTextToShow = if (quizItem.numCorrectResponsesInARow > 0) answeredText else ""
    val questionText = quizItem.qgCurrentPromptNumber + ": " + quizItem.prompt
    val fullQuestionText = questionText + wordTextToShow + answeredTextToShow
    output(s"$fullQuestionText\n")

    if (quizItem.useMultipleChoice) showChoicesAndProcessResponse(quizItem)
    else getTextResponseAndProcess(quizItem)
  }

  def showChoicesAndProcessResponse(
      quizItem: QuizItemViewWithChoices): State[Quiz, UserConsoleResponse] = {
    val choices = ChoiceGroup[String](quizItem.allChoices)
    choices.show()

    Try(choices.getSelectionFromInput).recover {
      case e: Exception => Invalid
    }.map(userResponse => processAnswer(userResponse, quizItem)).get
  }

  def getTextResponseAndProcess(quizItem: QuizItemViewWithChoices):
      State[Quiz, UserConsoleResponse] = {
    output("(Not multiple choice. Type it in.)")
    Try(getAnswerFromInput).recover {
      case e: Exception => Invalid
    }.map(userResponse => processAnswer(userResponse, quizItem)).get
  }

  def processAnswer(userResponse: UserConsoleResponse,
      quizItem: QuizItemViewWithChoices): State[Quiz, UserConsoleResponse] = for {
    quiz <- State.get[Quiz]
    _ <- State.put(userResponse match {
      case answer: Answer => processUserAnswer(quiz, answer.text, quizItem)
      case _ => quiz
    })
  } yield userResponse

  def processUserAnswer(
      quiz: Quiz, userResponse: String,
      quizItem: QuizItemViewWithChoices): Quiz = {
    val isCorrect = quiz.isCorrect(quizItem.quizGroupHeader, quizItem.prompt.value, userResponse)
    if (isCorrect) output("\nCorrect!\n")
    else output("\nWrong! It's " + quizItem.correctResponse + "\n")

    Util.stopwatch(quiz.updateWithUserResponse(
        isCorrect,
        quizItem.quizGroupHeader,
        quizItem.quizItem), "updateQuiz")
  }

  def getAnswerFromInput: UserConsoleResponse =
    readLineUntilNoBackspaces match {
      case "q" => Quit
      case "quit" => Quit
      case input: String => TextAnswer(input)
    }
}

