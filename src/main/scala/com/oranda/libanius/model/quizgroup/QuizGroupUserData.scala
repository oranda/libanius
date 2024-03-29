/*
 * Libanius
 * Copyright (C) 2012-2022 James McCabe <jjtmccabe@gmail.com>
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

import com.oranda.libanius.dependencies.AppDependencyAccess
import com.oranda.libanius.model.ModelComponent
import com.oranda.libanius.model.action.serialize.*
import com.oranda.libanius.model.action.serialize.CustomFormat.*
import com.oranda.libanius.model.action.serialize.CustomFormatForModelComponents.*
import scalaz.*

case class QuizGroupUserData(isActive: Boolean = false, currentPromptNumber: Int = 0) extends ModelComponent

object QuizGroupUserData extends AppDependencyAccess {
  def apply(headerLine: String): QuizGroupUserData =
    deserialize[QuizGroupUserData, ParamsNone](headerLine, ParamsNone())

  val activeLens: Lens[QuizGroupUserData, Boolean] = Lens.lensu(
    get = (_: QuizGroupUserData).isActive,
    set = (qgud: QuizGroupUserData, active: Boolean) => qgud.copy(isActive = active)
  )

  val promptNumberLens: Lens[QuizGroupUserData, Int] = Lens.lensu(
    get = (_: QuizGroupUserData).currentPromptNumber,
    set = (q: QuizGroupUserData, promptNum: Int) => q.copy(currentPromptNumber = promptNum)
  )
}
