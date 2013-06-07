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

import scala.xml.Unparsed
import scala.xml.Text
import scala.util.Random
import com.oranda.libanius.util.StringUtil
import com.sun.xml.internal.ws.util.StringUtils
import com.oranda.libanius.util.Platform
import com.oranda.libanius.model.ModelComponent
import scala.collection.mutable.ArrayBuffer

/*
 * A List is a bit faster than a Set when deserializing. High performance is required.
 * TODO: try again to convert this to a Set.
 */
case class WordMappingValueSet(values: List[WordMappingValue] = Nil) extends ModelComponent {    
  
  override def toString = values.toString
  
  def toXML = values map (_.toXML)
    
  // Example: contract:696,697;698/treaty:796;798
  def toCustomFormat(strBuilder: StringBuilder) = 
    StringUtil.mkString(strBuilder, values, wmvToCustomFormat, '/')
  
  def wmvToCustomFormat(strBuilder: StringBuilder, wmv: WordMappingValue) =
    wmv.toCustomFormat(strBuilder)
    
  def strings: Iterable[String] = values map (_.toString ) 
  
  def size = values.size
  
  def numItemsAndCorrectAnswers = Pair(size, numCorrectAnswers)
  
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
    WordMappingValueSet(values.filterNot(_ == wordMappingValue))
  
  def findPresentableWordMappingValue(currentPromptNumber: Int): Option[WordMappingValue] = {
    values.iterator.find(_.isPresentable(currentPromptNumber))
  }
  
  def findAnyUnfinishedWordMappingValue: Option[WordMappingValue] = 
    values.iterator.find(_.isUnfinished)    
    
  def findRandomWordValue(): String = {
    val randomIndex = Random.nextInt(values.size)
    val valueArray: Array[WordMappingValue] = values.toArray[WordMappingValue]
    valueArray(randomIndex).value
  }
  
  def findValue(value: String): Option[WordMappingValue] = values.find(_.value == value)
  
  def containsValue(value: String): Boolean = findValue(value).isDefined
  
  def valueBeginningWith(valueStart: String) = 
    values.find(_.value.startsWith(valueStart))
}


object WordMappingValueSet extends Platform {
  
  def combineValueSets(valueSets: Iterable[WordMappingValueSet]): List[WordMappingValue] =
    valueSets.foldLeft(new ArrayBuffer[WordMappingValue]()) {          
        (acc, wm) => acc ++ wm.values
    }.toList
  
  val wmvSplitter = getSplitter('/')
  // Example: contract:696,697;698/treaty:796;798
  def fromCustomFormat(str: String): WordMappingValueSet = {
    val values = new ArrayBuffer[WordMappingValue]()
    wmvSplitter.setString(str)
    while (wmvSplitter.hasNext)
      values += WordMappingValue.fromCustomFormat(wmvSplitter.next) 
    WordMappingValueSet(values.toList)  
  }
    
  /*
  def fromXML(node: xml.Node): WordMappingValueSet = {
    val values = new ArrayBuffer[WordMappingValue]()
    for (wordMappingValueXml <- node \\ "wordMappingValue")
	    values += WordMappingValue.fromXML(wordMappingValueXml)
    WordMappingValueSet(values.toList) 
  }
  */
  
}