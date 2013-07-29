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

  def makeFileName = keyType + "-" + valueType + ".wmg"

  def reverse = QuizGroupHeader(valueType, keyType)
}

object QuizGroupHeader {

  def apply(headerLine: String): QuizGroupHeader =
    this(parseKeyType(headerLine), parseValueType(headerLine))

  def parseKeyType(str: String) = StringUtil.parseValue(str, "keyType=\"", "\"")
  def parseValueType(str: String) = StringUtil.parseValue(str, "valueType=\"", "\"")
}
