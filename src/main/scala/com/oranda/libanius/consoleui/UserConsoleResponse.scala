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

import com.oranda.libanius.model.quizgroup.QuizGroupHeader

sealed trait UserConsoleResponse extends Any

sealed trait TextInput extends Any with UserConsoleResponse {
  def text: String
}

case class WordQuery(text: String) extends AnyVal with TextInput

sealed trait Answer extends Any with TextInput

case class TextAnswer(text: String) extends AnyVal with Answer
case class ChosenOptions[T](chosenOptions: List[T]) extends Answer {
  override def text = chosenOptions.iterator.next.toString
}
sealed trait NoProcessResponse extends UserConsoleResponse
case object Invalid extends NoProcessResponse
case object Quit extends NoProcessResponse
