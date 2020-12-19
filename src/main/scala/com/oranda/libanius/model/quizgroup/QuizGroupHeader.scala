/*
 * Libanius
 * Copyright (C) 2012-2020 James McCabe <jjtmccabe@gmail.com>
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
import com.oranda.libanius.model.action.serialize.CustomFormatParserFast._
import com.oranda.libanius.model.action.serialize._
import fastparse.all._
import fastparse.core.Parsed

import scala.collection.immutable.List

case class QuizGroupKey(promptType: String, responseType: String, quizGroupType: QuizGroupType) {
  override def toString = s"$quizGroupType: $promptType-$responseType"
  def reverse = QuizGroupKey(responseType, promptType, quizGroupType)
}

case class QuizGroupHeader(
  quizGroupKey: QuizGroupKey,
  mainSeparator: Separator,
  numCorrectResponsesRequired: Int,
  useMultipleChoiceUntil: Int
) extends ModelComponent {
  // promptType example: "English word"
  // responseType example: "German word"

  override def toString = quizGroupKey.toString

  def promptType = quizGroupKey.promptType
  def responseType = quizGroupKey.responseType
  def quizGroupType = quizGroupKey.quizGroupType

  def matches(other: QuizGroupKey) =
    promptType == other.promptType && responseType == other.responseType

  def makeQgFileName = s"$promptType-$responseType.qgr"
  def makeDictFileName = s"$promptType-$responseType.dct"

  def reverse = copy(quizGroupKey.reverse)

  def createQuizGroup(text: String): QuizGroup = {
    val Parsed.Success(qgh, _) = quizGroupWithHeader.parse(text)
    qgh.quizGroup
  }
}

object QuizGroupHeader extends AppDependencyAccess {

  val maxNumCorrectResponsesRequired = 12
  val allIntervals = List(0, 5, 15, 15, 60, 600, 1200, 2500, 5000, 10000, 20000, 40000)

  def apply(headerLine: String): QuizGroupHeader = {
    val Parsed.Success(qgh, _) = quizGroupHeader.parse(headerLine)
    qgh
  }

  def apply(
    promptType: String,
    responseType: String,
    quizGroupType: QuizGroupType,
    mainSeparator: String,
    numCorrectResponsesRequired: Int,
    useMultipleChoiceUntil: Int
  ): QuizGroupHeader = {
    val actualNumCorrectResponsesRequired =
      math.min(maxNumCorrectResponsesRequired, numCorrectResponsesRequired)
    QuizGroupHeader(
      QuizGroupKey(promptType, responseType, quizGroupType),
      Separator(mainSeparator),
      actualNumCorrectResponsesRequired,
      math.min(useMultipleChoiceUntil, actualNumCorrectResponsesRequired)
    )
  }
}

sealed abstract class QuizGroupType(val str: String) {
  // a default constructor is needed for serialization
  def this() = this("WordMapping")
}

object QuizGroupType {
  case object WordMapping extends QuizGroupType("WordMapping")
  case object QuestionAndAnswer extends QuizGroupType("QuestionAndAnswer")

  def fromString(qgType: String): QuizGroupType = qgType match {
    case "WordMapping" => WordMapping
    case "QuestionAndAnswer" => QuestionAndAnswer
  }
}

