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

package com.oranda.libanius.model.quizgroup

import scala.language.implicitConversions
import com.oranda.libanius.dependencies.AppDependencyAccess
import com.oranda.libanius.model.ModelComponent
import java.lang.StringBuilder

import com.oranda.libanius.model.action.serialize._
import CustomFormat._
import CustomFormatForModelComponents._

/*
 * Convenience class for passing around a key-value pair from the Quiz.quizGroups map.
 */
case class QuizGroupWithHeader(header: QuizGroupHeader, quizGroup: QuizGroup)
  extends ModelComponent {

  def toPair = (header, quizGroup)
  def toCustomFormat = serialize(this, new StringBuilder, new NoParams).toString
}

object QuizGroupWithHeader extends AppDependencyAccess {
  // Forward access to QuizGroupHeader whenever necessary
  implicit def qgwh2qgh(qgwh: QuizGroupWithHeader): QuizGroupHeader = qgwh.header

  // Forward access to QuizGroup whenever necessary
  implicit def qgwh2qg(qgwh: QuizGroupWithHeader): QuizGroup = qgwh.quizGroup
}
