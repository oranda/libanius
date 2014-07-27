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

import com.oranda.libanius.util.StringUtil
import com.oranda.libanius.dependencies.AppDependencyAccess

import com.oranda.libanius.model.{FromParamsWithSeparator, ModelComponent}
import com.oranda.libanius.model.CustomFormat._

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

    import com.oranda.libanius.model.CustomFormatForModelComponents.customFormatQuizGroupWithHeader

    val qgh: QuizGroupWithHeader = from(text, FromParamsWithSeparator("|"))
    qgh.quizGroup
  }

}

object QuizGroupHeader extends AppDependencyAccess {

  def apply(headerLine: String): QuizGroupHeader =
    this(parseQuizGroupType(headerLine), parsePromptType(headerLine),
        parseResponseType(headerLine), parseMainSeparator(headerLine),
        parseUseMultipleChoiceUntil(headerLine))

  def parseQuizGroupType(str: String): QuizGroupType =
    quizGroupType(StringUtil.parseValue(str, "type=\"", "\"").getOrElse(""))

  def quizGroupType(str: String): QuizGroupType =
    str match {
      case "WordMapping" => WordMapping
      case "QuestionAndAnswer" => QuestionAndAnswer
      case _ => l.logError("QuizGroupType " + str + " not recognized")
                QuestionAndAnswer
    }

  def parsePromptType(str: String): String =
    StringUtil.parseValue(str, "promptType=\"", "\"").getOrElse("")
  def parseResponseType(str: String): String =
    StringUtil.parseValue(str, "responseType=\"", "\"").getOrElse("")
  def parseMainSeparator(str: String): String =
    StringUtil.parseValue(str, "mainSeparator=\"", "\"").getOrElse("|")
  def parseUseMultipleChoiceUntil(str: String): Int =
    StringUtil.parseValue(str, "useMultipleChoiceUntil=\"", "\"").getOrElse("4").toInt

}

abstract class QuizGroupType
case object WordMapping extends QuizGroupType
case object QuestionAndAnswer extends QuizGroupType