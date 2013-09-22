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

import scala.util.Try
import scala.collection.mutable.ListBuffer

import com.oranda.libanius.util.StringUtil
import com.oranda.libanius.model.quizitem.QuizItem
import com.oranda.libanius.model.ModelComponent
import com.oranda.libanius.dependencies.AppDependencyAccess

import java.lang.StringBuilder
import scala.language.implicitConversions

/*
 * A List is a bit faster than a Set when deserializing. High performance is required.
 */
case class WordMappingValueSet(values: List[WordMappingValue] = Nil) extends ModelComponent {

  def updated(values: List[WordMappingValue]) = WordMappingValueSet(values)

  def addValueToEnd(value: WordMappingValue) = {
    val newValues = if (!values.contains(value)) values :+ value else values
    WordMappingValueSet(newValues)
  }

  def filterOut(value: String): WordMappingValueSet =
    updated(values.filterNot(_.value == value))

  def removeValue(value: String) =
    filterOut(value)

  override def toString = values.toString

  // Example: contract:696,697;698/treaty:796;798
  def toCustomFormat(strBuilder: StringBuilder) =
    StringUtil.mkString(strBuilder, values, wmvToCustomFormat, '/')

  def wmvToCustomFormat(strBuilder: StringBuilder, wmv: WordMappingValue): StringBuilder =
    wmv.toCustomFormat(strBuilder)

  def strings: Iterable[String] = values.map(_.value)

  def size = values.size

  def findValue(value: String): Option[String] = values.find(_.value == value).map(_.value)

  def containsValue(value: String): Boolean = findValue(value).isDefined

  def valueBeginningWith(valueStart: String) = values.find(_.value.startsWith(valueStart))

}

object WordMappingValueSet extends AppDependencyAccess {

  def apply(values: WordMappingValue*): WordMappingValueSet = WordMappingValueSet(values:_*)

  def createFromStrings(values: String*): WordMappingValueSet = WordMappingValueSet(values.map(
    WordMappingValue(_)).toList)
  def createFromQuizItems(quizItems: List[QuizItem]): WordMappingValueSet =
    WordMappingValueSet(quizItems.map(WordMappingValue(_)).toList)

  def combineValueSets(valueSets: Iterable[WordMappingValueSet]): List[WordMappingValue] =
    valueSets.flatMap(_.values).toList

  // Example: contract:696,697;698/treaty:796;798
  def fromCustomFormat(str: String): WordMappingValueSet = {
    val values = new ListBuffer[WordMappingValue]()
    val wmvsSplitter = stringSplitterFactory.getSplitter('/')
    def parseFromCustomFormat {
      wmvsSplitter.setString(str)
      while (wmvsSplitter.hasNext) {
        val nextVal = wmvsSplitter.next
        values += WordMappingValue.fromCustomFormat(nextVal)
      }
    }
    Try(parseFromCustomFormat) recover {
      case e: Exception => l.logError("WordMappingValueSet: Could not parse str " + str, e)
    }
    WordMappingValueSet(values.toList)
  }
}

abstract class WordMappingValueSetWrapperBase {
  def wmvs: WordMappingValueSet
}

/*
 * This object provides an implicit conversion so that calls to a proxy object are
 * automatically forwarded to the concrete entity that it wraps.
 */
object WordMappingValueSetWrapperBase {
  implicit def proxy2wmvs(proxy: WordMappingValueSetWrapperBase): WordMappingValueSet = proxy.wmvs
}

/*
 * Because deserializing a WordMappingValueSet is slow, it is wrapped in a Proxy that
 * supports lazy initialization.
 */
case class WordMappingValueSetLazyProxy(strValues: String)
    extends WordMappingValueSetWrapperBase {   
  
  lazy val wmvs: WordMappingValueSet = WordMappingValueSet.fromCustomFormat(strValues)
}

/*
 * When a new WordMappingValueSet is generated from a proxied WordMappingValueSet, it needs to be
 * wrapped in order to have a type compatible with the original WordMappingValueSetLazyProxy.
 */
case class WordMappingValueSetWrapper(wmvs: WordMappingValueSet)
    extends WordMappingValueSetWrapperBase

object WordMappingValueSetWrapper {
  def apply(values: List[String]): WordMappingValueSetWrapper =
    WordMappingValueSetWrapper(WordMappingValueSet(values.map(WordMappingValue(_))))
}
