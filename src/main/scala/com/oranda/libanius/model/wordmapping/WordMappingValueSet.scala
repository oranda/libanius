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

import scala.util.{Random}
import com.oranda.libanius.util.{StringUtil, Platform}
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer


/*
 * A List is a bit faster than a Set when deserializing. High performance is required.
 * TODO: try again to convert this to a Set.
 */
case class WordMappingValueSet(values: List[WordMappingValue] = Nil) {    
  
  override def toString = values.toString
    
  // Example: contract:696,697;698/treaty:796;798
  def toCustomFormat(strBuilder: StringBuilder) = 
    StringUtil.mkString(strBuilder, values, wmvToCustomFormat, '/')
  
  def wmvToCustomFormat(strBuilder: StringBuilder, wmv: WordMappingValue): StringBuilder =
    wmv.toCustomFormat(strBuilder)
    
  def strings: Iterable[String] = values map (_.toString ) 
  
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

  def replaceWmv(wmvNew: WordMappingValue): WordMappingValueSet =
    filterOut(wmvNew.value).addValueToFront(wmvNew)

  def addValueToFront(wordMappingValue: WordMappingValue): WordMappingValueSet = {
    val newValues =
      if (!values.contains(wordMappingValue)) wordMappingValue +: values
      else values
    WordMappingValueSet(newValues)    
  }
    
  def addValueToEnd(wordMappingValue: WordMappingValue): WordMappingValueSet = {
    val newValues = 
      if (!values.contains(wordMappingValue)) values :+ wordMappingValue
      else values
    WordMappingValueSet(newValues)    
  }
  
  def filterOut(value: String): WordMappingValueSet = 
    WordMappingValueSet(values.filterNot(_.value == value))
  
  def removeValue(wordMappingValue: WordMappingValue): WordMappingValueSet =
    filterOut(wordMappingValue.value)
  
  def findPresentableWordMappingValue(currentPromptNumber: Int): Option[WordMappingValue] =
    values.iterator.find(_.isPresentable(currentPromptNumber))

  def findAnyUnfinishedWordMappingValue: Option[WordMappingValue] = 
    values.iterator.find(_.isUnfinished)    
    
  def findRandomWordValue(): String = {
    val randomIndex = Random.nextInt(values.size)
    val valueArray: Array[WordMappingValue] = values.toArray[WordMappingValue]
    valueArray(randomIndex).value
  }
  
  def findValue(value: String): Option[WordMappingValue] = values.find(_.value == value)
  
  def containsValue(value: String): Boolean = findValue(value).isDefined
  
  def valueBeginningWith(valueStart: String) = values.find(_.value.startsWith(valueStart))
}


object WordMappingValueSet extends Platform {

  def combineValueSets(valueSets: Iterable[WordMappingValueSetWrapperBase]):
      List[WordMappingValue] =
    valueSets.foldLeft(new ArrayBuffer[WordMappingValue]()) {          
        (acc, wm) => acc ++ wm.values
    }.toList

  // Example: contract:696,697;698/treaty:796;798
  def fromCustomFormat(str: String): WordMappingValueSet = {

    val values = new ListBuffer[WordMappingValue]()
    val wmvsSplitter = getSplitter('/')
    try {
      wmvsSplitter.setString(str)
      while (wmvsSplitter.hasNext)
        values += WordMappingValue.fromCustomFormat(wmvsSplitter.next)
    } catch {
      case e: Exception => log("WordMappingValueSet: ERROR: Could not parse str " + str, e)
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
  def apply(values: List[WordMappingValue]): WordMappingValueSetWrapper =
    WordMappingValueSetWrapper(WordMappingValueSet(values))
}
