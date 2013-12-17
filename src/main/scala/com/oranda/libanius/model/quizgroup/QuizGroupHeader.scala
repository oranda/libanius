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

import com.oranda.libanius.util.StringUtil
import com.oranda.libanius.dependencies.AppDependencyAccess
import com.oranda.libanius.model.wordmapping.WordMappingGroup

import java.lang.StringBuilder
import com.oranda.libanius.model.ModelComponent

case class QuizGroupHeader(quizGroupType: QuizGroupType, promptType: String,
    responseType: String, mainSeparator: String) extends ModelComponent {
  // promptType example: "English word"
  // responseType example: "German word"

  override def toString = quizGroupType + ": " + promptType + "-" + responseType

  def toCustomFormat(strBuilder: StringBuilder) =
    strBuilder.append("quizGroup type=\"").append(quizGroupType).append("\" promptType=\"").
        append(promptType).append("\" responseType=\"").append(responseType).
        append("\" mainSeparator=\"").append(mainSeparator).append("\"")

  def matches(other: QuizGroupHeader) =
    promptType == other.promptType && responseType == other.responseType

  def makeQgFileName = promptType + "-" + responseType + ".qgr"
  def makeDictFileName = promptType + "-" + responseType + ".dct"

  def reverse = QuizGroupHeader(quizGroupType, responseType, promptType, mainSeparator)

  def createQuizGroup(text: String): QuizGroup =
    QuizGroupWithHeader.fromCustomFormat(text, mainSeparator).quizGroup
}

object QuizGroupHeader extends AppDependencyAccess {

  def apply(headerLine: String): QuizGroupHeader =
    this(parseQuizGroupType(headerLine), parsePromptType(headerLine),
        parseResponseType(headerLine), parseMainSeparator(headerLine))

  def parseQuizGroupType(str: String): QuizGroupType =
    quizGroupType(StringUtil.parseValue(str, "type=\"", "\"").getOrElse(""))

  def quizGroupType(str: String): QuizGroupType =
    str match {
      case "WordMapping" => WordMapping
      case "QuestionAndAnswer" => QuestionAndAnswer
      case _ => l.logError("QuizGroupType " + str + " not recognized")
                QuestionAndAnswer
    }

  def parsePromptType(str: String): String = StringUtil.parseValue(str, "promptType=\"", "\"").getOrElse("")
  def parseResponseType(str: String): String = StringUtil.parseValue(str, "responseType=\"", "\"").getOrElse("")
  def parseMainSeparator(str: String): String =
    StringUtil.parseValue(str, "mainSeparator=\"", "\"").getOrElse("|")

}

abstract class QuizGroupType
case object WordMapping extends QuizGroupType
case object QuestionAndAnswer extends QuizGroupType