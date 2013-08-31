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

import scala.util.{Random, Try}
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer

import com.oranda.libanius.dependencies.AppDependencies
import com.oranda.libanius.model.{QuizPair, QuizValueWithUserAnswers}
import com.oranda.libanius.util.StringUtil

/*
 * A List is a bit faster than a Set when deserializing. High performance is required.
 * TODO: try again to convert this to a Set.
 */
case class WordMappingValueSet(values: List[QuizValueWithUserAnswers] = Nil) {

  def updated(values: List[QuizValueWithUserAnswers]) = WordMappingValueSet(values)

  def addValueToEnd(wordMappingValue: QuizValueWithUserAnswers) = {
    val newValues =
      if (!values.contains(wordMappingValue)) values :+ wordMappingValue
      else values
    WordMappingValueSet(newValues)
  }

  def removeValue(wordMappingValue: QuizValueWithUserAnswers) =
    filterOut(wordMappingValue.value)

  override def toString = values.toString

  // Example: contract:696,697;698/treaty:796;798
  def toCustomFormat(strBuilder: StringBuilder) =
    StringUtil.mkString(strBuilder, values, wmvToCustomFormat, '/')

  def wmvToCustomFormat(strBuilder: StringBuilder, value: QuizValueWithUserAnswers):
      StringBuilder =
    value.toCustomFormat(strBuilder)

  def strings: Iterable[String] = values.map(_.toString )

  def size = values.size

  def numItemsAndCorrectAnswers: Pair[Int, Int] = Pair(size, numCorrectAnswers)

  def numCorrectAnswers = {
    /*
     * This functional version is about twice as slow as the version actually used:
     *
     * values.iterator.foldLeft(0)(_ + _.numCorrectAnswersInARow)
     */
    var numCorrectAnswers = 0
    values.foreach { wmv => numCorrectAnswers += wmv.numCorrectAnswersInARow }
    numCorrectAnswers
  }


  def replaceWmv(valueNew: QuizValueWithUserAnswers): WordMappingValueSet = {
    val quizValueSet: WordMappingValueSet = filterOut(valueNew.value)
    quizValueSet.addValueToFront(valueNew).asInstanceOf[WordMappingValueSet]
  }

  def addValueToFront(quizValue: QuizValueWithUserAnswers): WordMappingValueSet = {
    val newValues =
      if (!values.contains(quizValue)) quizValue +: values
      else values
    updated(newValues)
  }

  def filterOut(value: String): WordMappingValueSet =
    updated(values.filterNot(_.value == value))



  def findRandomWordValue(): String = {
    val randomIndex = Random.nextInt(values.size)
    val valueArray: Array[QuizValueWithUserAnswers] = values.toArray[QuizValueWithUserAnswers]
    valueArray(randomIndex).value
  }

  def findValue(value: String): Option[QuizValueWithUserAnswers] = values.find(_.value == value)

  def containsValue(value: String): Boolean = findValue(value).isDefined

  def valueBeginningWith(valueStart: String) = values.find(_.value.startsWith(valueStart))
}


object WordMappingValueSet {

  val l = AppDependencies.logger

  def apply(values: String*): WordMappingValueSet =
    WordMappingValueSet(values.map(QuizValueWithUserAnswers(_)).toList)

  def createFromQuizPairs(quizPairs: List[QuizPair]): WordMappingValueSet =
    WordMappingValueSet(quizPairs.map(_.value).toList)

  def combineValueSets(valueSets: Iterable[WordMappingValueSet]):
      List[QuizValueWithUserAnswers] =
    valueSets.flatMap(_.values).toList
    /*TODO: Test if this is more efficient:
     valueSets.foldLeft(new ArrayBuffer[QuizValueWithUserAnswers]()) {
      (acc, wm) => acc ++ wm.values
    }.toList */

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
