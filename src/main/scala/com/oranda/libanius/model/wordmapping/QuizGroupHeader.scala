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
package com.oranda.libanius.model.wordmapping

import com.oranda.libanius.util.StringUtil
import com.oranda.libanius.model.ModelComponent


case class QuizGroupHeader(keyType: String, valueType: String) extends ModelComponent {
  // keyType example: "English word"
  // valueType example: "German word"

  override def toString = keyType + "-" + valueType

  def toCustomFormat(strBuilder: StringBuilder) =
    strBuilder.append("wordMappingGroup keyType=\"").append(keyType).
        append("\" valueType=\"").append(valueType).append("\"")

  def matches(other: QuizGroupHeader) =
    keyType == other.keyType && valueType == other.valueType

  def makeWmgFileName = keyType + "-" + valueType + ".wmg"
  def makeDictFileName = keyType + "-" + valueType + ".dct"

  def reverse = QuizGroupHeader(valueType, keyType)
}

object QuizGroupHeader {

  def apply(headerLine: String): QuizGroupHeader =
    this(parseKeyType(headerLine), parseValueType(headerLine))

  def parseKeyType(str: String) = StringUtil.parseValue(str, "keyType=\"", "\"")
  def parseValueType(str: String) = StringUtil.parseValue(str, "valueType=\"", "\"")
}
