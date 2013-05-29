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

import scala.collection.immutable.HashSet

import com.oranda.libanius.model.Quiz
import com.oranda.libanius.util.StringUtil
import com.oranda.libanius.Props

import math.BigDecimal.double2bigDecimal

class QuizOfWordMappings(_currentPromptNumber: Int) 
    extends Quiz(_currentPromptNumber) {
  
  private var wordMappingGroups = Set[WordMappingGroupReadWrite]()
  
  def this() = this(_currentPromptNumber = 0)

  def toXML  =
    <quizOfWordMappings currentPromptNumber={ _currentPromptNumber.toString }>
      { wordMappingGroups map (wmg => wmg.toXML) }
    </quizOfWordMappings>
      
  /*
   * Example:
   * quizOfWordMappings currentPromptNumber="0"
   *   wordMappingGroup valueType="English word" keyType="German word"
   *     against|wider
   */
  def toCustomFormat = {
    // For efficiency, avoiding Scala's mkString 
    val strBuilder = new StringBuilder("quizOfWordMappings currentPromptNumber=\"").
        append(_currentPromptNumber).append("\"\n")
    StringUtil.mkString(strBuilder, wordMappingGroups, wmgToCustomFormat, '\n')
  }  
  
  def wmgToCustomFormat(strBuilder: StringBuilder, wmg: WordMappingGroupReadWrite) = 
    wmg.toCustomFormat(strBuilder)
    
  def findOrAddWordMappingGroup(keyType: String, valueType: String): 
      WordMappingGroupReadWrite = {
    val wordMappingGroupOpt = findWordMappingGroup(keyType, valueType)    
    wordMappingGroupOpt match {
      case Some(wordMappingGroup) => wordMappingGroup
      case None => addWordMappingGroup(new WordMappingGroupReadWrite(keyType, valueType))
    }    
  }
    
  def findValuesFor(keyWord: String, keyType: String, valueType: String): 
      Iterable[String] = {
    findWordMappingGroup(keyType, valueType).foreach(
      _.findValuesFor(keyWord).foreach(wordMappingValueSet =>
        return wordMappingValueSet.strings        
    ))    
    new HashSet[String]
  }
  
  def findWordMappingGroup(keyType: String, valueType: String): 
      Option[WordMappingGroupReadWrite] =    
    wordMappingGroups.find(wordMappingGroup =>
        keyType == wordMappingGroup.keyType && valueType == wordMappingGroup.valueType)
  
  
  def addWordMappingGroup(wordMappingGroup: WordMappingGroupReadWrite): WordMappingGroupReadWrite = {
    wordMappingGroups += wordMappingGroup
    wordMappingGroup
  }
  
  def removeWordMappingGroup(keyType: String, valueType: String) {
    findWordMappingGroup(keyType, valueType).foreach(wordMappingGroups -= _)
  }

  def removeWord(keyWord: String, keyType: String, valueType: String): Boolean = {
    val wordMappingGroupOpt = findWordMappingGroup(keyType, valueType)
    wordMappingGroupOpt.isDefined && 
        wordMappingGroupOpt.get.removeWordMapping(keyWord) != None
  }
  
  def removeWordMappingValue(keyWord: String, wordMappingValue: WordMappingValue,
      keyType: String, valueType: String): Boolean = {
    val wordMappingGroupOpt = findWordMappingGroup(keyType, valueType)
    wordMappingGroupOpt.isDefined && wordMappingGroupOpt.get.
        removeWordMappingValue(keyWord, wordMappingValue)
  }

  def findQuizItem: Option[QuizItemViewWithOptions] =
    /*
     * Just find the first "presentable" word mapping and return it immediately.
     * .iterator is considered to be more efficient than .view here.
     */
    wordMappingGroups.iterator.map(_.findPresentableQuizItem(currentPromptNumber)).
        find(_.isDefined).getOrElse(None)  

  def addWordMappingToFrontOfTwoGroups(keyType: String, valueType: String, 
      keyWord: String, value: String) {
    // E.g. add to the English -> German group
    addWordMappingToFront(keyType, valueType, keyWord, value)
    // E.g. add to the German -> English group
    addWordMappingToFront(valueType, keyType, value, keyWord)
    
    def addWordMappingToFront(keyType: String, valueType: String, 
        keyWord: String, value: String) {
      findWordMappingGroup(keyType, valueType).foreach(_.addWordMappingToFront(
          keyWord, value))
    }
  }
  
  def numGroups = wordMappingGroups.size
  
  def numKeyWords = wordMappingGroups.foldLeft(0)(_ + _.numKeyWords)
  
  def numItems: Int = wordMappingGroups.foldLeft(0)(_ + _.numValues)
  
  override def scoreSoFar: BigDecimal = {  // out of 1.0
    val _numItemsAndCorrectAnswers = numItemsAndCorrectAnswers
    val scoreSoFar = _numItemsAndCorrectAnswers._2.toDouble / 
        (_numItemsAndCorrectAnswers._1 * Props.NUM_CORRECT_ANSWERS_REQUIRED).toDouble
    scoreSoFar
  } 
  
  def numCorrectAnswers = numItemsAndCorrectAnswers._2
    
  def numItemsAndCorrectAnswers = 
    wordMappingGroups.foldLeft(Pair(0, 0))((acc, group) =>
        (acc._1 + group.numItemsAndCorrectAnswers._1,
         acc._2 + group.numItemsAndCorrectAnswers._2))
         
  def merge(otherQuiz: QuizOfWordMappings) {
    otherQuiz.wordMappingGroups.foreach { otherWmg =>
      val wmg = findOrAddWordMappingGroup(otherWmg.keyType, otherWmg.valueType)
      wmg.merge(otherWmg)
    }      
  }
}

object QuizOfWordMappings {
  def fromCustomFormat(str: String): QuizOfWordMappings =
    new QuizOfWordMappings(
	  _currentPromptNumber = parseCurrentPromptNumber(str)) { 
      val wmgStrs = str.split("wordMappingGroup").tail
      wmgStrs.foreach { wmgStr => 
        addWordMappingGroup(WordMappingGroupReadWrite.fromCustomFormat(wmgStr))
      }
    }

  // Demo data to use as a fallback if no file is available
  def demoDataInCustomFormat =
    "quizOfWordMappings currentPromptNumber=\"0\"\n" +
    "wordMappingGroup keyType=\"English word\" valueType=\"German word\"\n" +
    "en route|unterwegs\n" +
    "contract|Vertrag\n" +
    "treaty|Vertrag\n" +
    "against|wider\n" +
    "entertain|unterhalten\n" +
    "wordMappingGroup keyType=\"German word\" valueType=\"English word\"\n" +
    "unterwegs|en route\n" +
    "Vertrag|contract/treaty\n" +
    "wider|against\n" +
    "unterhalten|entertain"
 
  
  def parseCurrentPromptNumber(str: String) = 
    StringUtil.parseValue(str, "currentPromptNumber=\"", "\"").toInt
    
  def fromXML(node: xml.Node): QuizOfWordMappings =
    new QuizOfWordMappings(
	  _currentPromptNumber = (node \ "@currentPromptNumber").text.toInt) {
	  for (wordMappingGroupXml <- node \\ "wordMappingGroup")
	    addWordMappingGroup(WordMappingGroupReadWrite.fromXML(wordMappingGroupXml))	      
      }
}