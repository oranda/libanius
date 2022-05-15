/*
 * Libanius
 * Copyright (C) 2012-2022 James McCabe <jjtmccabe@gmail.com>
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

import com.oranda.libanius.consoleui.ConsoleUtil.*
import com.oranda.libanius.consoleui.Output.*
import com.oranda.libanius.dependencies.*
import com.oranda.libanius.model.*
import com.oranda.libanius.model.action.*
import com.oranda.libanius.model.action.QuizItemSource.*
import com.oranda.libanius.model.action.modelComponentsAsQuizItemSources.*
import com.oranda.libanius.model.quizgroup.QuizGroupType.WordMapping
import com.oranda.libanius.model.quizgroup.{QuizGroup, QuizGroupHeader}
import com.oranda.libanius.model.quizitem.QuizItemViewWithChoices
import com.oranda.libanius.util.{StringUtil, Util}
import scalaz.*

import scala.util.Try

trait InteractiveQuiz extends App with AppDependencyAccess {

  def userQuizGroupSelection(quizGroupHeaders: List[QuizGroupHeader]): Map[QuizGroupHeader, QuizGroup] = {
    output("Choose quiz group(s). For more than one, separate with commas, e.g. 1,2,3")
    val choices = ChoiceGroupQgHeaders(quizGroupHeaders)
    choices.show()

    val selectedQuizGroupHeaders: List[QuizGroupHeader] = choices.getSelectionFromInput match {
      case Right(ChosenOptions(selectedChoices)) => selectedChoices
      case _                                     => Nil
    }

    if selectedQuizGroupHeaders.isEmpty then {
      output("Unrecognized option")
      userQuizGroupSelection(quizGroupHeaders)
    } else selectedQuizGroupHeaders.map(header => (header, dataStore.loadQuizGroupCore(header))).toMap
  }

  def testUserWithQuizItem(quiz: Quiz): Unit = {
    showScore(quiz)
    Util.stopwatch(FindQuizItem.run(quiz), "find quiz items") match {
      case Some(quizItem) => keepShowingQuizItems(quiz, quizItem)
      case _              => output("No more questions found! Done!")
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

  // This can be overriden to disable saving for trivial cases.
  def saveQuiz(quiz: Quiz): Quiz = dataStore.saveQuiz(quiz)

  def showScore(quiz: Quiz): Unit = {
    val score: BigDecimal = Util.stopwatch(quiz.scoreSoFar, "scoreSoFar")
    val formattedScore    = StringUtil.formatScore(score)
    output(s"Score: $formattedScore")
  }

  def showQuizItemAndProcessResponse(quizItem: QuizItemViewWithChoices): State[Quiz, UserConsoleResponse] = {
    val wordText = s": what is the ${quizItem.responseType} for this ${quizItem.promptType}?"
    val wordTextToShow =
      if quizItem.quizGroupHeader.quizGroupType == WordMapping then wordText else ""
    val answeredText       = s" (correctly answered ${quizItem.numCorrectResponsesInARow} times)"
    val answeredTextToShow = if quizItem.numCorrectResponsesInARow > 0 then answeredText else ""
    val questionText       = s"${quizItem.qgCurrentPromptNumber}: ${quizItem.prompt}"
    val fullQuestionText   = questionText + wordTextToShow + answeredTextToShow
    output(s"$fullQuestionText\n")

    if quizItem.useMultipleChoice then showChoicesAndProcessResponse(quizItem)
    else getTextResponseAndProcess(quizItem)
  }

  def showChoicesAndProcessResponse(quizItem: QuizItemViewWithChoices): State[Quiz, UserConsoleResponse] = {
    val choices = ChoiceGroupStrings(quizItem.allChoices)
    choices.show()
    val userResponse = choices.getSelectionFromInput match {
      case Right(chosenOptions)    => chosenOptions
      case Left(noProcessResponse) => noProcessResponse
    }
    processAnswer(userResponse, quizItem)
  }

  def getTextResponseAndProcess(quizItem: QuizItemViewWithChoices): State[Quiz, UserConsoleResponse] = {
    output("(Not multiple choice. Type it in.)")
    Try(getAnswerFromInput).recover { case e: Exception =>
      Invalid
    }.map(userResponse => processAnswer(userResponse, quizItem)).get
  }

  def processAnswer(
    userResponse: UserConsoleResponse,
    quizItem: QuizItemViewWithChoices
  ): State[Quiz, UserConsoleResponse] = for
    quiz <- State.get[Quiz]
    _ <- State.put(userResponse match {
           case answer: Answer => processUserAnswer(quiz, answer.text, quizItem)
           case _              => quiz
         })
  yield userResponse

  def processUserAnswer(quiz: Quiz, userResponse: String, quizItem: QuizItemViewWithChoices): Quiz = {
    val (quizGroupKey, prompt) = (quizItem.quizGroupKey, quizItem.prompt.value)
    val isCorrect              = quiz.isCorrect(quizGroupKey, prompt, userResponse) == Correct
    if isCorrect then output("\nCorrect!\n")
    else output("\nWrong! It's " + quizItem.correctResponse + "\n")

    Util.stopwatch(quiz.updateWithUserResponse(isCorrect, quizItem.quizGroupHeader, quizItem.quizItem), "updateQuiz")
  }

  def getAnswerFromInput: UserConsoleResponse =
    readLineUntilNoBackspaces match {
      case "q" | "quit"  => Quit
      case input: String => TextAnswer(input)
    }
}
