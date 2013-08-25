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

case class OptionGroup[T](options: List[T]) {
  val optionsWithIndex = options.zipWithIndex

  def show() {
    optionsWithIndex.foreach {
      case (header, index) => output((index + 1).toString + ". " + header.toString)
    }
  }

  // return None if quit is requested
  def getSelectionFromInput: UserResponse =
    readLine match {
      case "q" => Quit
      case "quit" => Quit
      case userResponse: String =>
        Try(Options(userResponse.split(",").map(_.toInt - 1).map(options(_)).toList)).recover {
          case e: Exception => Invalid
        }.get
    }
}