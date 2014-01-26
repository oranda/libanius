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

package com.oranda.libanius.consoleui

import com.oranda.libanius.consoleui.Output._
import com.oranda.libanius.model.Quiz
import com.oranda.libanius.model.quizgroup.{QuizGroup, QuizGroupHeader}
import com.oranda.libanius.dependencies.AppDependencyAccess

object RunDictionary extends App with AppDependencyAccess {

  runDictionary()

  private[this] def runDictionary() {

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

  private[this] def userQuizGroupSelection(quizGroupHeaders: List[QuizGroupHeader]):
      Map[QuizGroupHeader, QuizGroup] = {
    output("Choose quiz group(s). For more than one, separate with commas, e.g. 1,2,3")
    val choices = ChoiceGroup[QuizGroupHeader](quizGroupHeaders)
    choices.show()

    def selectedQuizGroupHeaders = choices.getSelectionFromInput match {
      case ChosenOptions(selectedChoices) => selectedChoices.asInstanceOf[List[QuizGroupHeader]]
      case _ => List[QuizGroupHeader]()
    }

    selectedQuizGroupHeaders.map(header => (header, dataStore.loadQuizGroup(header))).toMap
  }

  private[this] def serveUserRequest(quiz: Quiz) {
    output("Word to search for: ")
    getWordQueryFromInput match {
      case Quit =>
        output("Exiting")
        System.exit(0)
      case WordQuery(text) =>
        searchAndDisplayResults(text, quiz)
        serveUserRequest(quiz)
    }
  }

  private[this] def searchAndDisplayResults(text: String, quiz: Quiz) = {
    val localResults = quiz.searchLocalDictionary(text)
    output(localResults.map(_.toString).mkString("\n"))
    val networkResults = quiz.searchRemoteDictionary(text)
    output(networkResults.map(_.toString).mkString("\n"))
  }

  private[this] def getWordQueryFromInput: UserConsoleResponse =
    ConsoleUtil.readLineUntilNoBackspaces match {
      case "q" => Quit
      case "quit" => Quit
      case input: String => WordQuery(input)
    }
}
