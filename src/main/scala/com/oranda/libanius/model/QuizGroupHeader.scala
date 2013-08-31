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
import com.oranda.libanius.dependencies.AppDependencies
import com.oranda.libanius.model.wordmapping.WordMappingGroup


case class QuizGroupHeader(quizGroupType: QuizGroupType, keyType: String, valueType: String)
    extends ModelComponent {
  // keyType example: "English word"
  // valueType example: "German word"

  override def toString = quizGroupType + ": " + keyType + "-" + valueType

  def toCustomFormat(strBuilder: StringBuilder) =
    strBuilder.append("quizGroup type=\"").append(quizGroupType).append("\" keyType=\"").
        append(keyType).append("\" valueType=\"").append(valueType).append("\"")

  def matches(other: QuizGroupHeader) =
    keyType == other.keyType && valueType == other.valueType

  def makeQgFileName = keyType + "-" + valueType + ".qg"
  def makeDictFileName = keyType + "-" + valueType + ".dct"

  def reverse = QuizGroupHeader(quizGroupType, valueType, keyType)

  // TODO: improve this
  def createQuizGroup(text: String): QuizGroup = quizGroupType match {
    case WordMapping => WordMappingGroup.fromCustomFormat(text).toQuizGroup
    case QuestionAndAnswer => QuizGroup.fromCustomFormat(text)
  }
}

object QuizGroupHeader {

  private[this] val l = AppDependencies.logger

  def apply(headerLine: String): QuizGroupHeader =
    this(parseQuizGroupType(headerLine), parseKeyType(headerLine),
        parseValueType(headerLine))

  def parseQuizGroupType(str: String): QuizGroupType = {
    StringUtil.parseValue(str, "type=\"", "\"") match {
      case "WordMapping" => WordMapping
      case "QuestionAndAnswer" => QuestionAndAnswer
      case _ => l.logError("QuizGroupType " + str + " not recognized")
                QuestionAndAnswer
    }
  }

  def parseKeyType(str: String): String = StringUtil.parseValue(str, "keyType=\"", "\"")
  def parseValueType(str: String): String = StringUtil.parseValue(str, "valueType=\"", "\"")
}

abstract class QuizGroupType
case object WordMapping extends QuizGroupType
case object QuestionAndAnswer extends QuizGroupType