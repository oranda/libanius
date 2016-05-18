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

import Output._
import scala.util.Try

/*
 * A list of choices in a console UI.
 */
case class ChoiceGroup[T](choices: List[T]) {
  val choicesWithIndex = choices.zipWithIndex

  def show(): Unit =
    choicesWithIndex.foreach {
      case (header, index) => output((index + 1).toString + ". " + header.toString)
    }

  def getSelectionFromInput: UserConsoleResponse =
    scala.io.StdIn.readLine match {
      case "q" => Quit
      case "quit" => Quit
      case userResponse: String =>
        Try(ChosenOptions(userResponse.split(",").map(_.toInt - 1).map(choices(_)).toList)).recover {
          case e: Exception => Invalid
        }.get
    }
}