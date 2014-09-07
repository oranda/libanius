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

import com.oranda.libanius.dependencies.AppDependencyAccess

import com.oranda.libanius.model._
import com.oranda.libanius.model.action.serialize._
import CustomFormat._
import CustomFormatForModelComponents._


case class QuizGroupHeader(quizGroupType: QuizGroupType, promptType: String,
    responseType: String, mainSeparator: String, useMultipleChoiceUntil: Int)
  extends ModelComponent {
  // promptType example: "English word"
  // responseType example: "German word"

  override def toString = quizGroupType + ": " + promptType + "-" + responseType

  def matches(other: QuizGroupHeader) =
    promptType == other.promptType && responseType == other.responseType

  def makeQgFileName = promptType + "-" + responseType + ".qgr"
  def makeDictFileName = promptType + "-" + responseType + ".dct"

  def reverse = QuizGroupHeader(quizGroupType, responseType, promptType, mainSeparator,
      useMultipleChoiceUntil)

  def createQuizGroup(text: String): QuizGroup = {

    import CustomFormatForModelComponents._

    val qgh: QuizGroupWithHeader =
        deserialize[QuizGroupWithHeader, Separator](text, Separator("|"))
    qgh.quizGroup
  }

}

object QuizGroupHeader extends AppDependencyAccess {

  def apply(headerLine: String): QuizGroupHeader =
      deserialize[QuizGroupHeader, NoParams](headerLine, NoParams())
}

abstract class QuizGroupType
case object WordMapping extends QuizGroupType
case object QuestionAndAnswer extends QuizGroupType