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
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer

import com.oranda.libanius.dependencies.AppDependencies
import com.oranda.libanius.model.{QuizValueWithUserAnswers, QuizValueSet}

/*
 * A List is a bit faster than a Set when deserializing. High performance is required.
 * TODO: try again to convert this to a Set.
 */
case class WordMappingValueSet(override val values: List[QuizValueWithUserAnswers] = Nil)
    extends QuizValueSet(values) {

  type MyType = WordMappingValueSet

  override def updated(values: List[QuizValueWithUserAnswers]) =
    WordMappingValueSet(values)

  override def addValueToEnd(wordMappingValue: QuizValueWithUserAnswers) = {
    val newValues =
      if (!values.contains(wordMappingValue)) values :+ wordMappingValue
      else values
    WordMappingValueSet(newValues)
  }

  def removeValue(wordMappingValue: QuizValueWithUserAnswers) =
    filterOut(wordMappingValue.value)
}


object WordMappingValueSet {

  val l = AppDependencies.logger

  def combineValueSets(valueSets: Iterable[WordMappingValueSet]):
      List[QuizValueWithUserAnswers] =
    valueSets.foldLeft(new ArrayBuffer[QuizValueWithUserAnswers]()) {
      (acc, wm) => acc ++ wm.values
    }.toList

  // Example: contract:696,697;698/treaty:796;798
  def fromCustomFormat(str: String): WordMappingValueSet = {

    val values = new ListBuffer[QuizValueWithUserAnswers]()
    val wmvsSplitter = AppDependencies.stringSplitterFactory.getSplitter('/')
    def parseFromCustomFormat {
      wmvsSplitter.setString(str)
      while (wmvsSplitter.hasNext)
        values += QuizValueWithUserAnswers.fromCustomFormat(wmvsSplitter.next)
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
case class WordMappingValueSetLazyProxy(valuesString: String) 
    extends WordMappingValueSetWrapperBase {   
  
  lazy val wmvs: WordMappingValueSet = WordMappingValueSet.fromCustomFormat(valuesString)
}

/*
 * When a new WordMappingValueSet is generated from a proxied WordMappingValueSet, it needs to be
 * wrapped in order to have a type compatible with the original WordMappingValueSetLazyProxy.
 */
case class WordMappingValueSetWrapper(wmvs: WordMappingValueSet)
    extends WordMappingValueSetWrapperBase

object WordMappingValueSetWrapper {
  def apply(values: List[QuizValueWithUserAnswers]): WordMappingValueSetWrapper =
    WordMappingValueSetWrapper(WordMappingValueSet(values))
}
