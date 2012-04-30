/*
 * Copyright 2012 James McCabe <jamesc@oranda.com>
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
import scala.collection.immutable.HashSet

case class WordMappingValueSet {
    
  // There will only be a small number of values
  var values: Set[WordMappingValue] = new HashSet()
  
  def toXML = {values map (wmv => wmv.toXML ) }   
  
  def strings: Set[String] = {values map (wmv => wmv.toString ) }
  
  def size = values.size
  
  def numCorrectAnswers : Int = values.foldLeft(0)(_ + _.numCorrectAnswersInARow) 
    
  def addWordMappingValue(wordMappingValueOpt: Option[WordMappingValue]) {
    values += wordMappingValueOpt.getOrElse(wordMappingValueOpt.get)
  } 
  
  def findPresentableWordMappingValue(numCorrectAnswersInARowDesired: Int,
      diffInPromptNumMinimum: Int, currentPromptNumber: Int): 
      Option[WordMappingValue] = {
                
    values.find(wmv => wmv.isPresentable(
        numCorrectAnswersInARowDesired, diffInPromptNumMinimum, currentPromptNumber))
  }

    
  def findRandomWordValue(): String = {
    val randomIndex = Random.nextInt(values.size)
    val valueArray: Array[WordMappingValue] = values.toArray[WordMappingValue]
    valueArray(randomIndex).value
  }
  
  def containsValue(value: String): Boolean = values.find(_.value == value).isDefined
  
}

object WordMappingValueSet {
  def fromXML(node: xml.Node): WordMappingValueSet = {
	new WordMappingValueSet() {
	  for (val wordMappingValueXml <- node \\ "wordMappingValue")
	    addWordMappingValue(Some(WordMappingValue.fromXML(wordMappingValueXml)))
	}
  }
  
}