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

package com.oranda.libanius.model.quizgroup

import scala.util.Try
import com.oranda.libanius.util.StringUtil

import scalaz._
import com.oranda.libanius.dependencies.AppDependencyAccess
import com.oranda.libanius.model.ModelComponent

case class QuizGroupUserData(isActive: Boolean = false, currentPromptNumber: Int = 0)
    extends ModelComponent

object QuizGroupUserData extends AppDependencyAccess {

  def apply(headerLine: String): QuizGroupUserData =
    this(parseIsActive(headerLine), parseCurrentPromptNumber(headerLine))

  def parseIsActive(str: String): Boolean =
    Try(StringUtil.parseValue(str, "isActive=\"", "\"").get.toBoolean).recover {
      case e: Exception => l.logError("Could not parse isActive from " + str)
        false
    }.get

  def parseCurrentPromptNumber(str: String): Int =
    Try(StringUtil.parseValue(str, "currentPromptNumber=\"", "\"").get.toInt).recover {
      case e: Exception => l.logError("Could not parse prompt number from " + str)
        0
    }.get

  val activeLens: Lens[QuizGroupUserData, Boolean] = Lens.lensu(
      get = (_: QuizGroupUserData).isActive,
      set = (qgud: QuizGroupUserData, active: Boolean) => qgud.copy(isActive = active))

  val promptNumberLens: Lens[QuizGroupUserData, Int] = Lens.lensu(
      get = (_: QuizGroupUserData).currentPromptNumber,
      set = (q: QuizGroupUserData, promptNum: Int) => q.copy(currentPromptNumber = promptNum))
}