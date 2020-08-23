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

case class QuizGroupHeader(
    quizGroupType: QuizGroupType,
    promptType: String,
    responseType: String,
    mainSeparator: Separator,
    useMultipleChoiceUntil: Int)
  extends ModelComponent {
  // promptType example: "English word"
  // responseType example: "German word"

  override def toString = s"$quizGroupType: $promptType-$responseType"

  def matches(other: QuizGroupHeader) =
    promptType == other.promptType && responseType == other.responseType

  def makeQgFileName = s"$promptType-$responseType.qgr"
  def makeDictFileName = s"$promptType-$responseType.dct"

  def reverse = QuizGroupHeader(quizGroupType, responseType, promptType, mainSeparator,
      useMultipleChoiceUntil)

  def createQuizGroup(text: String): QuizGroup = {
    val Parsed.Success(qgh, _) = quizGroupWithHeader.parse(text)
    qgh.quizGroup
  }

}

object QuizGroupHeader extends AppDependencyAccess {

  def apply(headerLine: String): QuizGroupHeader = {
    val Parsed.Success(qgh, _) = quizGroupHeader.parse(headerLine)
    qgh
  }

  def apply(quizGroupType: QuizGroupType, promptType: String,
      responseType: String, sep: String, useMultipleChoiceUntil: Int): QuizGroupHeader =
    QuizGroupHeader(quizGroupType, promptType, responseType, Separator(sep), useMultipleChoiceUntil)
}

sealed abstract class QuizGroupType(val str: String)
case object WordMapping extends QuizGroupType("WordMapping")
case object QuestionAndAnswer extends QuizGroupType("QuestionAndAnswer")

object QuizGroupType {
  def fromString(qgType: String): QuizGroupType = qgType match {
    case "WordMapping" => WordMapping
    case "QuestionAndAnswer" => QuestionAndAnswer
  }
}

