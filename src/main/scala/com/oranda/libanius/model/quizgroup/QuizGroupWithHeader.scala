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

package com.oranda.libanius.model.quizgroup

import scala.language.implicitConversions
import com.oranda.libanius.model.quizitem.{QuizItem, QuizItemViewWithChoices}
import com.oranda.libanius.dependencies.AppDependencyAccess
import com.oranda.libanius.util.Util

/*
 * Convenience class for passing around a key-value pair from the Quiz.quizGroups map.
 */
case class QuizGroupWithHeader(header: QuizGroupHeader, quizGroup: QuizGroup) extends AppDependencyAccess {
  def toPair = Pair(header, quizGroup)

  def findPresentableQuizItem: Option[QuizItemViewWithChoices] = {
    l.log("QuizGroupWithHeader findPresentableQuizItem")
    quizGroup.findPresentableQuizItem.map(quizItemWithChoices(_))
  }

  def findAnyUnfinishedQuizItem: Option[QuizItemViewWithChoices] =
    quizGroup.findAnyUnfinishedQuizItem.map(quizItemWithChoices(_))

  protected[model] def quizItemWithChoices(quizItem: QuizItem):
      QuizItemViewWithChoices = {
    val numCorrectAnswers = quizItem.userResponses.numCorrectResponsesInARow
    val useMultipleChoice = QuizItemViewWithChoices.useMultipleChoice(numCorrectAnswers)
    val falseAnswers =
      if (useMultipleChoice)
        Util.stopwatch(quizGroup.constructWrongChoices(quizItem, numCorrectAnswers),
            "constructWrongChoices")
      else Set[String]()
    new QuizItemViewWithChoices(quizItem, quizGroup.currentPromptNumber,
        header, falseAnswers, numCorrectAnswers)
  }
}

object QuizGroupWithHeader extends AppDependencyAccess {
  // Forward access to QuizGroupHeader whenever necessary
  implicit def qgwh2qgh(qgwh: QuizGroupWithHeader): QuizGroupHeader = qgwh.header

  // Forward access to QuizGroup whenever necessary
  implicit def qgwh2qg(qgwh: QuizGroupWithHeader): QuizGroup = qgwh.quizGroup

  def fromCustomFormat(text: String, mainSeparator: String = "|"): QuizGroupWithHeader = {
    val headerLine = text.takeWhile(_ != '\n')
    val qgHeader = QuizGroupHeader(headerLine)
    val qg = QuizGroup.fromCustomFormat(text, mainSeparator, headerLine)
    QuizGroupWithHeader(qgHeader, qg)
  }
}

