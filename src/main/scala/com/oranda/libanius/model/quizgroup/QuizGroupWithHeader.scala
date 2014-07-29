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

import scala.language.implicitConversions
import com.oranda.libanius.model.quizitem.{QuizItem}
import com.oranda.libanius.dependencies.AppDependencyAccess
import com.oranda.libanius.util.Util
import com.oranda.libanius.model.{ParamsDefault, CustomFormatForModelComponents, CustomFormat, ModelComponent}
import com.oranda.libanius.model.quizitem.QuizItemViewWithChoices
import java.lang.StringBuilder

import CustomFormat._
import CustomFormatForModelComponents._

/*
 * Convenience class for passing around a key-value pair from the Quiz.quizGroups map.
 */
case class QuizGroupWithHeader(header: QuizGroupHeader, quizGroup: QuizGroup)
    extends ModelComponent {
  def toPair = Pair(header, quizGroup)

  def findPresentableQuizItem: Option[QuizItemViewWithChoices] =
    quizGroup.findPresentableQuizItem.map(quizItemWithChoices(_, quizGroup.numLevels))

  def findAnyUnfinishedQuizItem: Option[QuizItemViewWithChoices] =
    quizGroup.findAnyUnfinishedQuizItem.map(quizItemWithChoices(_, quizGroup.numLevels))

  protected[model] def quizItemWithChoices(quizItem: QuizItem, numCorrectResponsesRequired: Int):
      QuizItemViewWithChoices = {
    val numCorrectResponses = quizItem.userResponses.numCorrectResponsesInARow
    /*
     * A quiz item might be presented initially in multiple-choice format,
     * then later wihout any such assistance.
     */
    val useMultipleChoice = numCorrectResponses < header.useMultipleChoiceUntil
    val falseAnswers =
      if (useMultipleChoice)
        Util.stopwatch(quizGroup.constructWrongChoices(quizItem, numCorrectResponses),
            "constructWrongChoices")
      else Nil
    new QuizItemViewWithChoices(quizItem, quizGroup.currentPromptNumber,
        header, falseAnswers, numCorrectResponses, numCorrectResponsesRequired,
        useMultipleChoice)
  }

  def toCustomFormat: String = {
    to(this, new StringBuilder, new ParamsDefault).toString
  }
}

object QuizGroupWithHeader extends AppDependencyAccess {
  // Forward access to QuizGroupHeader whenever necessary
  implicit def qgwh2qgh(qgwh: QuizGroupWithHeader): QuizGroupHeader = qgwh.header

  // Forward access to QuizGroup whenever necessary
  implicit def qgwh2qg(qgwh: QuizGroupWithHeader): QuizGroup = qgwh.quizGroup
}

