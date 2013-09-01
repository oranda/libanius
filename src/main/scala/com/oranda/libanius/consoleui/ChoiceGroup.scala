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

import Output._
import scala.util.Try

/*
 * A list of choices in a console UI.
 */
case class ChoiceGroup[T](choices: List[T]) {
  val choicesWithIndex = choices.zipWithIndex

  def show() {
    choicesWithIndex.foreach {
      case (header, index) => output((index + 1).toString + ". " + header.toString)
    }
  }

  def getSelectionFromInput: UserConsoleResponse =
    readLine match {
      case "q" => Quit
      case "quit" => Quit
      case userResponse: String =>
        Try(ChosenOptions(userResponse.split(",").map(_.toInt - 1).map(choices(_)).toList)).recover {
          case e: Exception => Invalid
        }.get
    }
}