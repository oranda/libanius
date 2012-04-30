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

import com.oranda.libanius.Props
import java.io.FileOutputStream
import scala.collection.immutable.List
import scala.collection.immutable.HashSet
import com.oranda.libanius.model.Quiz
import android.util.Log

class QuizOfWordMappings(_currentPromptNumber: Int) extends Quiz(_currentPromptNumber) {
  
  private[this] var wordMappingGroups = Set[WordMappingGroup]()

  def findOrAddWordMappingGroup(keyType: String, valueType: String): WordMappingGroup = {
    val wordMappingGroupOpt = findWordMappingGroup(keyType, valueType)    
    wordMappingGroupOpt match {
      case Some(wordMappingGroup) => wordMappingGroup
      case None => addWordMappingGroup(new WordMappingGroup(keyType, valueType))
    }    
  }
    
  def findValuesFor(keyWord: String, keyType: String, valueType: String): Set[String] = {
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
      Option[WordMappingGroup] = {
    
    wordMappingGroups.find(wordMappingGroup =>
        keyType == wordMappingGroup.keyType && valueType == wordMappingGroup.valueType)
  }
  
  def addWordMappingGroup(wordMappingGroup: WordMappingGroup): WordMappingGroup = {
      wordMappingGroups += wordMappingGroup
      wordMappingGroup
  }

  def deleteWord(keyWord: String, keyType: String, valueType: String): Boolean = {
    val wordMappingGroupOpt = findWordMappingGroup(keyType, valueType)
    wordMappingGroupOpt.isDefined && wordMappingGroupOpt.get.remove(keyWord) != None
  }

  def findQuizItem(numCorrectAnswersInARowDesired: Int, diffInPromptNum: Int): 
      Option[QuizItemViewWithOptions] = {
    /*
     * Just find the first "presentable" word mapping and return it immediately
     * .iterator is considered to be more efficient than .view here 
     */
    wordMappingGroups.iterator.map(_.findPresentableQuizItem(
        numCorrectAnswersInARowDesired, diffInPromptNum, currentPromptNumber)).
        find(_.isDefined).getOrElse(None)   
  }

  def numGroups = wordMappingGroups.size
  
  def numKeyWords = wordMappingGroups.foldLeft(0)(_ + _.numKeyWords)
  
  def numItems : Int = wordMappingGroups.foldLeft(0)(_ + _.numValues)
  
  def numCorrectAnswers : Int = 
    wordMappingGroups.foldLeft(0)(_ + _.numCorrectAnswers)      

  def toXML =
    <quizOfWordMappings currentPromptNumber={ currentPromptNumber.toString }>
      { wordMappingGroups map (wmg => wmg.toXML) }
    </quizOfWordMappings>
}

object QuizOfWordMappings {
  def fromXML(node: xml.Node): QuizOfWordMappings =
    new QuizOfWordMappings(
	  _currentPromptNumber = (node \ "@currentPromptNumber").text.toInt) {
	  for (val wordMappingGroupXml <- node \\ "wordMappingGroup")
	    addWordMappingGroup(WordMappingGroup.fromXML(wordMappingGroupXml))	      
      }
}