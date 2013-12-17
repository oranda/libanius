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
import com.oranda.libanius.util.{StringUtil, Util}
import Output._
import com.oranda.libanius.model._
import com.oranda.libanius.dependencies._
import com.oranda.libanius.model.quizitem.QuizItemViewWithChoices
import com.oranda.libanius.model.quizgroup.{WordMapping, QuizGroupHeader, QuizGroup}

trait InteractiveQuiz extends App with AppDependencyAccess {

  def userQuizGroupSelection(quizGroupHeaders: List[QuizGroupHeader]):
      Map[QuizGroupHeader, QuizGroup] = {
    output("Choose quiz group(s). For more than one, separate with commas, e.g. 1,2,3")
    val choices = ChoiceGroup[QuizGroupHeader](quizGroupHeaders)
    choices.show()

    def selectedQuizGroupHeaders = choices.getSelectionFromInput match {
      case ChosenOptions(selectedChoices) => selectedChoices.asInstanceOf[List[QuizGroupHeader]]
      case _ => List[QuizGroupHeader]()
    }

    selectedQuizGroupHeaders.map(header => (header, dataStore.loadQuizGroupCore(header))).toMap
  }

  def testUserWithQuizItem(quiz: Quiz) {
    showScore(quiz)
    Util.stopwatch(quiz.findPresentableQuizItem, "find quiz items") match {
      case (Some((quizItem, qgWithHeader))) =>
        val updatedQuiz = quiz.updatedPromptNumber(qgWithHeader)
        keepShowingQuizItems(updatedQuiz, quizItem)
      case _ =>
        output("No more questions found! Done!")
    }
  }

  def keepShowingQuizItems(quiz: Quiz, quizItem: QuizItemViewWithChoices) {
    showQuizItemAndProcessResponse(quiz, quizItem) match {
      case (Invalid, updatedQuiz) =>
        output("Invalid input\n")
        keepShowingQuizItems(updatedQuiz, quizItem)
      case (Quit, updatedQuiz) =>
        output("Exiting")
        saveQuiz(updatedQuiz)
        System.exit(0)
      case (_, updatedQuiz) =>
        testUserWithQuizItem(updatedQuiz)
    }
  }

  def saveQuiz(quiz: Quiz) {
    dataStore.saveQuiz(quiz, path = conf.filesDir)
  }

  def showScore(quiz: Quiz) {
    val score: BigDecimal = Util.stopwatch(quiz.scoreSoFar, "scoreSoFar")
    val formattedScore = StringUtil.formatScore(score)
    output("Score: " + formattedScore)
  }

  def showQuizItemAndProcessResponse(quiz: Quiz, quizItem: QuizItemViewWithChoices):
      (UserConsoleResponse, Quiz) = {
    val wordText = ": what is the " + quizItem.responseType + " for this " +
        quizItem.promptType + "?"
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
    val correctAnswer = quizItem.correctResponse
    val isCorrect = correctAnswer.looselyMatches(userAnswerTxt)
    if (isCorrect) output("\nCorrect!\n") else output("\nWrong! It's " + correctAnswer + "\n")

    Util.stopwatch(quiz.updateWithUserResponse(isCorrect, quizItem.quizGroupHeader,
        quizItem.quizItem), "updateQuiz")
  }

  def getAnswerFromInput: UserConsoleResponse =
    readLineUntilNoBackspaces match {
      case "q" => Quit
      case "quit" => Quit
      case input: String => TextAnswer(input)
    }

  /*
   * Predef readLine does not allow backspacing. This is a hack until libanius SBT is
   * upgraded to a version that is not incompatible with jline 2.11.
   */
  def readLineUntilNoBackspaces: String = {
    val s = readLine
    val extendedCode: (Char) => Boolean = (c:Char) => (c == 127)
    if (!s.exists(extendedCode)) s else {
      println("backspace detected: try again")
      readLineUntilNoBackspaces
    }
  }
}

