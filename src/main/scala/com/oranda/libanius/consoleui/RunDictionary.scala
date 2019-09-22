/*
 * Libanius
 * Copyright (C) 2012-2019 James McCabe <james@oranda.com>
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

import com.oranda.libanius.consoleui.Output._
import com.oranda.libanius.model.{SearchResult, Quiz}
import com.oranda.libanius.model.quizgroup.{QuizGroup, QuizGroupHeader}
import com.oranda.libanius.dependencies.AppDependencyAccess
import scala.util.{Failure, Success, Try}

object RunDictionary extends App with AppDependencyAccess {

  runDictionary()

  private[this] def runDictionary(): Unit = {

    output("Loading dictionaries...")

    val availableQuizGroups = dataStore.findAvailableQuizGroups
    val quiz =
      if (!availableQuizGroups.isEmpty)
        Quiz(userQuizGroupSelection(availableQuizGroups.toList))
      else {
        output("No quiz groups found. Defaulting to dummy data.\n")
        Quiz.demoQuiz()
      }
    output("Dictionaries have been initialized. To quit, type q at any time.\n")
    serveUserRequest(quiz)
  }

  private[this] def userQuizGroupSelection(
      quizGroupHeaders: List[QuizGroupHeader]): Map[QuizGroupHeader, QuizGroup] = {
    output("Choose quiz group(s). For more than one, separate with commas, e.g. 1,2,3")
    val choices = ChoiceGroupQgHeaders(quizGroupHeaders)
    choices.show()

    def selectedQuizGroupHeaders = choices.getSelectionFromInput match {
      case Right(ChosenOptions(selectedChoices)) => selectedChoices
      case _ => Nil
    }

    selectedQuizGroupHeaders.map(header => (header, dataStore.loadQuizGroup(header))).toMap
  }

  private[this] def serveUserRequest(quiz: Quiz): Unit = {
    output("Word to search for: ")
    getWordQueryFromInput match {
      case Quit =>
        output("Exiting")
        System.exit(0)
      case WordQuery(text) =>
        searchAndDisplayResults(text, quiz)
        serveUserRequest(quiz)
      case _ =>
        serveUserRequest(quiz)
    }
  }

  private[this] def searchAndDisplayResults(text: String, quiz: Quiz) = {
    output(resultsToString(quiz.searchLocalDictionary(text)))
    output(resultsToString(quiz.searchRemoteDictionary(text)))
  }

  private[this] def resultsToString(results: Try[List[SearchResult]]) =
    results match {
      case Success(results) => results.map(_.toString).mkString("\n")
      case Failure(ex) =>
          s"Problem getting results: ${ex.getClass.getSimpleName}  ${ex.getMessage}"
    }

  private[this] def getWordQueryFromInput: UserConsoleResponse =
    ConsoleUtil.readLineUntilNoBackspaces match {
      case "q" | "quit" => Quit
      case input: String => WordQuery(input)
    }
}
