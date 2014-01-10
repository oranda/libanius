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
case class WordMappingValueSet(values: List[WordMappingValue] = Nil)
    extends ModelComponent {

  def updated(values: List[WordMappingValue]) = WordMappingValueSet(values)

  def addValueToEnd(value: WordMappingValue) = {
    val newValues = if (!values.contains(value)) values :+ value else values
    WordMappingValueSet(newValues)
  }

  def filterOut(value: String): WordMappingValueSet =
    updated(values.filterNot(_.value == value))

  def removeValue(value: String) = filterOut(value)

  override def toString = values.toString

  // Example: contract:696,697;698/treaty:796;798
  def toCustomFormat(strBuilder: StringBuilder, mainSeparator: String) = {

    def wmvToCustomFormat(strBuilder: StringBuilder, wmv: WordMappingValue): StringBuilder =
      wmv.toCustomFormat(strBuilder, mainSeparator)

    StringUtil.mkString(strBuilder, values, wmvToCustomFormat, '/')
  }



  def strings: Iterable[String] = values.map(_.value)

  def size = values.size

  def findValue(value: String): Option[String] = values.find(_.value == value).map(_.value)

  def containsValue(value: String): Boolean = findValue(value).isDefined

  def valueBeginningWith(valueStart: String) = values.find(_.value.startsWith(valueStart))

}

object WordMappingValueSet extends AppDependencyAccess {

  //def apply(mainSeparator: String, values: WordMappingValue*): WordMappingValueSet =
  //  WordMappingValueSet(mainSeparator, values:_*)

  def createFromStrings(values: String*): WordMappingValueSet =
    WordMappingValueSet(values.map(WordMappingValue(_)).toList)

  def createFromQuizItems(quizItems: List[QuizItem], mainSeparator: String): WordMappingValueSet =
    WordMappingValueSet(quizItems.map(WordMappingValue(_)).toList)

  def combineValueSets(valueSets: Iterable[WordMappingValueSet]): List[WordMappingValue] =
    valueSets.flatMap(_.values).toList

  // Example: contract:696,697;698/treaty:796;798
  def fromCustomFormat(str: String, mainSeparator: String): WordMappingValueSet = {
    val values = new ListBuffer[WordMappingValue]()
    val wmvsSplitter = stringSplitterFactory.getSplitter('/')
    def parseFromCustomFormat {
      wmvsSplitter.setString(str)
      while (wmvsSplitter.hasNext) {
        val nextVal = wmvsSplitter.next
        values += WordMappingValue.fromCustomFormat(nextVal, mainSeparator)
      }
    }
    Try(parseFromCustomFormat) recover {
      case e: Exception => l.logError("WordMappingValueSet: Could not parse text " + str, e)
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
case class WordMappingValueSetLazyProxy(strValues: String, mainSeparator: String)
    extends WordMappingValueSetWrapperBase {   
  
  lazy val wmvs: WordMappingValueSet =
    WordMappingValueSet.fromCustomFormat(strValues, mainSeparator)
}

/*
 * When a new WordMappingValueSet is generated from a proxied WordMappingValueSet, it needs to be
 * wrapped in order to have a type compatible with the original WordMappingValueSetLazyProxy.
 */
case class WordMappingValueSetWrapper(wmvs: WordMappingValueSet, mainSeparator: String)
    extends WordMappingValueSetWrapperBase

object WordMappingValueSetWrapper {
  def apply(values: List[String], mainSeparator: String): WordMappingValueSetWrapper = {
    val wmvs = values.map(WordMappingValue(_)).toList
    WordMappingValueSetWrapper(WordMappingValueSet(wmvs), mainSeparator)
  }

}
