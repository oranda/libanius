/*
 * Copyright 2012 James McCabe <james@oranda.com>
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

import com.oranda.libanius.Props
import java.io.FileOutputStream
import scala.collection.immutable.List
import scala.collection.immutable.HashSet
import com.oranda.libanius.model.Quiz
import android.util.Log
import com.oranda.libanius.util.StringUtil

class QuizOfWordMappings(_currentPromptNumber: Int) 
    extends Quiz(_currentPromptNumber) {
  
  private[this] var wordMappingGroups = Set[WordMappingGroup]()

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
    // For efficiency, avoiding Scala's own StringBuilder and mkString 
    val strBuilder = new StringBuilder("quizOfWordMappings currentPromptNumber=\"").
        append(_currentPromptNumber).append("\"\n")
    StringUtil.mkString(strBuilder, wordMappingGroups, wmgToCustomFormat, '\n')
  }  
  
  def wmgToCustomFormat(strBuilder: StringBuilder, wmg: WordMappingGroup) = 
    wmg.toCustomFormat(strBuilder)
    
  def findOrAddWordMappingGroup(keyType: String, valueType: String): WordMappingGroup = {
    val wordMappingGroupOpt = findWordMappingGroup(keyType, valueType)    
    wordMappingGroupOpt match {
      case Some(wordMappingGroup) => wordMappingGroup
      case None => addWordMappingGroup(new WordMappingGroup(keyType, valueType))
    }    
  }
    
  def findValuesFor(keyWord: String, keyType: String, valueType: String): Iterable[String] = {
    val wordMappingGroupOpt = findWordMappingGroup(keyType, valueType)
    wordMappingGroupOpt match {
      case Some(wordMappingGroup) =>
          val wordMappingValueSetOpt = wordMappingGroup.findValuesFor(keyWord)
          wordMappingValueSetOpt match {
            case Some(wordMappingValueSet) => return wordMappingValueSet.strings        
            case None =>
          }
      case None =>    
    }
    new HashSet[String]
  }
  
  def findWordMappingGroup(keyType: String, valueType: String): 
      Option[WordMappingGroup] =    
    wordMappingGroups.find(wordMappingGroup =>
        keyType == wordMappingGroup.keyType && valueType == wordMappingGroup.valueType)
  
  
  def addWordMappingGroup(wordMappingGroup: WordMappingGroup): WordMappingGroup = {
      wordMappingGroups += wordMappingGroup
      wordMappingGroup
  }

  def deleteWord(keyWord: String, keyType: String, valueType: String): Boolean = {
    val wordMappingGroupOpt = findWordMappingGroup(keyType, valueType)
    wordMappingGroupOpt.isDefined && wordMappingGroupOpt.get.remove(keyWord) != None
  }
  
  def deleteWordMappingValue(keyWord: String, wordMappingValue: WordMappingValue,
      keyType: String, valueType: String): Boolean = {
    val wordMappingGroupOpt = findWordMappingGroup(keyType, valueType)
    wordMappingGroupOpt.isDefined && wordMappingGroupOpt.get.
        deleteWordMappingValue(keyWord, wordMappingValue)
  }
  

  def findQuizItem(/*numCorrectAnswersInARowDesired: Int, diffInPromptNum: Int*/): 
      Option[QuizItemViewWithOptions] =
    /*
     * Just find the first "presentable" word mapping and return it immediately.
     * .iterator is considered to be more efficient than .view here.
     */
    wordMappingGroups.iterator.map(_.findPresentableQuizItem(currentPromptNumber)).
        find(_.isDefined).getOrElse(None)  

  def numGroups = wordMappingGroups.size
  
  def numKeyWords = wordMappingGroups.foldLeft(0)(_ + _.numKeyWords)
  
  def numItems : Int = wordMappingGroups.foldLeft(0)(_ + _.numValues)
  
  override def scoreSoFar : BigDecimal = {  // out of 1.0
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
}

object QuizOfWordMappings {
  def fromCustomFormat(str: String): QuizOfWordMappings =
    new QuizOfWordMappings(
	  _currentPromptNumber = parseCurrentPromptNumber(str)) { 
      val wmgStrs = str.split("wordMappingGroup").tail
      for (wmgStr <- wmgStrs)
        addWordMappingGroup(WordMappingGroup.fromCustomFormat(wmgStr)) 
    }

  def parseCurrentPromptNumber(str: String) = 
    StringUtil.parseValue(str, "currentPromptNumber=\"", "\"").toInt
    
  def fromXML(node: xml.Node): QuizOfWordMappings =
    new QuizOfWordMappings(
	  _currentPromptNumber = (node \ "@currentPromptNumber").text.toInt) {
	  for (wordMappingGroupXml <- node \\ "wordMappingGroup")
	    addWordMappingGroup(WordMappingGroup.fromXML(wordMappingGroupXml))	      
      }
}