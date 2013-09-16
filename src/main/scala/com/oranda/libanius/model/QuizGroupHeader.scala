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

import com.oranda.libanius.util.StringUtil
import com.oranda.libanius.dependencies.AppDependencyAccess
import com.oranda.libanius.model.wordmapping.WordMappingGroup

import java.lang.StringBuilder

case class QuizGroupHeader(quizGroupType: QuizGroupType, promptType: String, responseType: String)
    extends ModelComponent {
  // promptType example: "English word"
  // responseType example: "German word"

  override def toString = quizGroupType + ": " + promptType + "-" + responseType

  def toCustomFormat(strBuilder: StringBuilder) =
    strBuilder.append("quizGroup type=\"").append(quizGroupType).append("\" promptType=\"").
        append(promptType).append("\" responseType=\"").append(responseType).append("\"")

  def matches(other: QuizGroupHeader) =
    promptType == other.promptType && responseType == other.responseType

  def makeQgFileName = promptType + "-" + responseType + ".qg"
  def makeDictFileName = promptType + "-" + responseType + ".dct"

  def reverse = QuizGroupHeader(quizGroupType, responseType, promptType)

  def getSaveData: SaveData = {
    val serialized = toCustomFormat(new StringBuilder())
    val fileName = makeQgFileName
    SaveData(fileName, serialized.toString)
  }

  // TODO: improve the QuestionAndAnswer case
  def createQuizGroup(text: String): QuizGroup = quizGroupType match {
    case WordMapping => WordMappingGroup.fromCustomFormat(text).toQuizGroup
    case QuestionAndAnswer => QuizGroup.fromCustomFormat(text).quizGroup
  }
}

object QuizGroupHeader extends AppDependencyAccess {

  def apply(headerLine: String): QuizGroupHeader =
    this(parseQuizGroupType(headerLine), parsePromptType(headerLine),
        parseResponseType(headerLine))

  def parseQuizGroupType(str: String): QuizGroupType = {
    StringUtil.parseValue(str, "type=\"", "\"") match {
      case "WordMapping" => WordMapping
      case "QuestionAndAnswer" => QuestionAndAnswer
      case _ => l.logError("QuizGroupType " + str + " not recognized")
                QuestionAndAnswer
    }
  }

  def parsePromptType(str: String): String = StringUtil.parseValue(str, "promptType=\"", "\"")
  def parseResponseType(str: String): String = StringUtil.parseValue(str, "responseType=\"", "\"")
}

abstract class QuizGroupType
case object WordMapping extends QuizGroupType
case object QuestionAndAnswer extends QuizGroupType