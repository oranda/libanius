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

import scala.collection.immutable._

import com.oranda.libanius.model.Quiz
import com.oranda.libanius.util.StringUtil
import com.oranda.libanius.Conf

import math.BigDecimal.double2bigDecimal

case class QuizOfWordMappings(currentPromptNumber: Int = 0, 
    wordMappingGroups: Set[WordMappingGroupReadWrite] = ListSet())  
    extends Quiz(currentPromptNumber) {
  
  def this() = this(currentPromptNumber = 0, 
      wordMappingGroups = ListSet[WordMappingGroupReadWrite]())

  def copy(newPromptNumber: Int) = new QuizOfWordMappings(currentPromptNumber, wordMappingGroups)
      
  /*
   * Example:
   * quizOfWordMappings currentPromptNumber="0"
   *   wordMappingGroup valueType="English word" keyType="German word"
   *     against|wider
   */
  def toCustomFormat = {
    // For efficiency, avoiding Scala's mkString 
    val strBuilder = new StringBuilder("quizOfWordMappings currentPromptNumber=\"").
        append(currentPromptNumber).append("\"\n")
    StringUtil.mkString(strBuilder, wordMappingGroups, wmgToCustomFormat, '\n')
  }  
  
  def wmgToCustomFormat(strBuilder: StringBuilder, wmg: WordMappingGroupReadWrite) = 
    wmg.toCustomFormat(strBuilder)
    
  def findOrAddWordMappingGroup(keyType: String, valueType: String): 
      (QuizOfWordMappings, WordMappingGroupReadWrite) = {
    val wordMappingGroup = findWordMappingGroup(keyType, valueType)    
    wordMappingGroup match {
      case Some(wordMappingGroup) => (this, wordMappingGroup)
      case None => val wordMappingGroup = WordMappingGroupReadWrite(keyType, valueType)
                   (addWordMappingGroup(wordMappingGroup), wordMappingGroup)
    }    
  }
     
  def findValuesFor(keyWord: String, keyType: String, valueType: String): 
      Iterable[String] = {
    findWordMappingGroup(keyType, valueType).foreach(
      _.findValuesFor(keyWord).foreach(wordMappingValueSet =>
        return wordMappingValueSet.strings.toList        
    ))    
    new HashSet[String]
  }
  
  def findWordMappingGroup(keyType: String, valueType: String): 
      Option[WordMappingGroupReadWrite] =
    wordMappingGroups.find(wordMappingGroup =>
        keyType == wordMappingGroup.keyType && valueType == wordMappingGroup.valueType)
  
  def removeWordMappingGroup(keyType: String, valueType: String): QuizOfWordMappings = {
    val wordMappingGroup = findWordMappingGroup(keyType, valueType)
    val wordMappingGroupsFiltered = wordMappingGroups.filterNot(Some(_) == wordMappingGroup)
    new QuizOfWordMappings(currentPromptNumber, wordMappingGroupsFiltered)
  }

  // This will replace any existing wordMappingGroup with the same key-value pair  
  def addWordMappingGroup(wmg: WordMappingGroupReadWrite): QuizOfWordMappings = {
    val newQuiz = removeWordMappingGroup(wmg.keyType, wmg.valueType)
    new QuizOfWordMappings(currentPromptNumber, ListSet(wmg) ++ newQuiz.wordMappingGroups)
  }

  // Just a synonym for addWordMappingGroup
  def replaceWordMappingGroup(wmg: WordMappingGroupReadWrite) = addWordMappingGroup(wmg)

  def removeWord(keyWord: String, keyType: String, valueType: String): QuizOfWordMappings = {
    val wordMappingGroup = findWordMappingGroup(keyType, valueType)
    wordMappingGroup match {
      case Some(wmg) => replaceWordMappingGroup(wmg.removeWordMapping(keyWord))
      case None => this
    }
  }
  
  def removeWordMappingValue(keyWord: String, wordMappingValue: WordMappingValue,
      keyType: String, valueType: String): (QuizOfWordMappings, Boolean) = {
    val wordMappingGroup = findWordMappingGroup(keyType, valueType)
    wordMappingGroup match {
      case Some(wmg) => 
        val (newWmg, wasRemoved) = wmg.removeWordMappingValue(keyWord, wordMappingValue)
        (replaceWordMappingGroup(newWmg), wasRemoved)
      case None => (this, false)
    }
  }

  def findQuizItem: Option[QuizItemViewWithOptions] =
    /*
     * Just find the first "presentable" word mapping and return it immediately.
     * .iterator is considered to be more efficient than .view here.
     */
    wordMappingGroups.iterator.map(_.findPresentableQuizItem(currentPromptNumber)).
        find(_.isDefined).getOrElse(None)

  def addWordMappingToFront(keyType: String, valueType: String,
                            keyWord: String, value: String): QuizOfWordMappings = {
    val wmg = findWordMappingGroup(keyType, valueType)
    wmg match {
      case Some(wmg) => replaceWordMappingGroup(wmg.addWordMappingToFront(keyWord, value))
      case None => this
    }
  }

  def addWordMappingToFrontOfTwoGroups(keyType: String, valueType: String, 
      keyWord: String, value: String): QuizOfWordMappings = {

    // E.g. add to the English -> German group
    val quizAfter1stChange = addWordMappingToFront(keyType, valueType, keyWord, value)
    
    // E.g. add to the German -> English group
    val quizAfter2ndChange = quizAfter1stChange.addWordMappingToFront(
        valueType, keyType, value, keyWord)
    
    quizAfter2ndChange
  }
  
  def numGroups = wordMappingGroups.size
  
  def numKeyWords = wordMappingGroups.foldLeft(0)(_ + _.numKeyWords)
  
  def numItems: Int = wordMappingGroups.foldLeft(0)(_ + _.numValues)
  
  override def scoreSoFar: BigDecimal = {  // out of 1.0
    val _numItemsAndCorrectAnswers = numItemsAndCorrectAnswers
    val scoreSoFar = _numItemsAndCorrectAnswers._2.toDouble / 
        (_numItemsAndCorrectAnswers._1 * Conf.conf.numCorrectAnswersRequired).toDouble
    scoreSoFar
  } 
  
  def numCorrectAnswers = numItemsAndCorrectAnswers._2
    
  def numItemsAndCorrectAnswers = 
    wordMappingGroups.foldLeft(Pair(0, 0))((acc, group) =>
        (acc._1 + group.numItemsAndCorrectAnswers._1,
         acc._2 + group.numItemsAndCorrectAnswers._2))
         
  def merge(otherQuiz: QuizOfWordMappings): QuizOfWordMappings = {
    val wordMappingGroupsCombined = otherQuiz.wordMappingGroups.foldLeft(wordMappingGroups) { 
      (acc, otherWmg) => 
        val wmg = findWordMappingGroup(otherWmg.keyType, otherWmg.valueType)
        val wmgMerged = otherWmg.merge(wmg)
        wordMappingGroups.filterNot(Some(_) == wmg) + wmgMerged
    }      
    new QuizOfWordMappings(currentPromptNumber, wordMappingGroupsCombined)
  }
}

object QuizOfWordMappings {
  def fromCustomFormat(str: String): QuizOfWordMappings = {
    
    var quiz = QuizOfWordMappings(currentPromptNumber = parseCurrentPromptNumber(str)) 
    
    val wmgStrs = str.split("wordMappingGroup").tail
    wmgStrs.foreach { wmgStr => 
      quiz = quiz.addWordMappingGroup(WordMappingGroupReadWrite.fromCustomFormat(wmgStr))
    }
        
    quiz
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
 
  def parseCurrentPromptNumber(str: String): Int =
    StringUtil.parseValue(str, "currentPromptNumber=\"", "\"").toInt

}