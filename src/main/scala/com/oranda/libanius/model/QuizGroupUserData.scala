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

package com.oranda.libanius.model

import scala.util.Try
import com.oranda.libanius.util.StringUtil
import java.lang.StringBuilder

import scalaz._
import scalaz.std.set
import scalaz.Scalaz._
import com.oranda.libanius.dependencies.AppDependencyAccess

case class QuizGroupUserData(isActive: Boolean = false, currentPromptNumber: Int = 0)
    extends ModelComponent {

  def toCustomFormat(strBuilder: StringBuilder) = {
    strBuilder.append(" currentPromptNumber=\"").
        append(currentPromptNumber).append("\"").append(" isActive=\"").
        append(isActive).append("\"")
    strBuilder
  }
}

object QuizGroupUserData extends AppDependencyAccess {

  def apply(headerLine: String): QuizGroupUserData =
    this(parseIsActive(headerLine), parseCurrentPromptNumber(headerLine))

  def parseIsActive(str: String): Boolean =
    Try(StringUtil.parseValue(str, "isActive=\"", "\"").toBoolean).recover {
      case e: Exception => l.logError("Could not parse isActive from " + str)
        false
    }.get

  def parseCurrentPromptNumber(str: String): Int =
    Try(StringUtil.parseValue(str, "currentPromptNumber=\"", "\"").toInt).recover {
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