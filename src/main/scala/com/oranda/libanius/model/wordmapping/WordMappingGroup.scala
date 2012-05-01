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

import scala.collection.immutable.HashSet
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable
import scala.util.Random

import android.util.Log

case class WordMappingGroup(val keyType: String, val valueType: String) {
  // keyType example: "English word"
  // valueType example: "German word"
  
  // When building a map from a large amount of data, the mutable collection is faster 
  private[this] val wordMappings = new mutable.HashMap[String, WordMappingValueSet]
  
  def toXML =
    <wordMappingGroup keyType={keyType} valueType={valueType}>
      {wordMappings map (keyValue => 
          <wordMapping key={keyValue._1}>{keyValue._2.toXML}</wordMapping> ) }
    </wordMappingGroup>
  
  def numKeyWords = wordMappings.size

  def numValues = wordMappings.values.foldLeft(0)(_ + _.size)
  
  def numCorrectAnswers : Int = 
    wordMappings.values.foldLeft(0)(_ + _.numCorrectAnswers)
  
  def contains(wordMapping: String): Boolean = wordMappings.contains(wordMapping)

  // not a lazy val because values can be deleted
  def allWordMappingValues =
    wordMappings.values.foldLeft(new ArrayBuffer[WordMappingValue]()) {          
        (acc, wm) => acc ++ wm.values
    }
  
  def addWordMapping(key: String, value: String) {
    if (key.toLowerCase == value.toLowerCase)
      return
    val existingValuesOpt = wordMappings.get(key)
    val existingValues = existingValuesOpt match {
        case Some(existingValues) => existingValues
        case None => val wmvs = new WordMappingValueSet()
          wordMappings.put(key, wmvs)
          wmvs
    }          
    existingValues.addWordMappingValue(Some(new WordMappingValue(value)))
  }
  
  def addWordMapping(key: String, wordMappingValueSetOpt: Option[WordMappingValueSet]) {
    if (wordMappingValueSetOpt.isDefined)
      wordMappings.put(key, wordMappingValueSetOpt.get)
  }
 
  def remove(key: String): Option[WordMappingValueSet] = wordMappings.remove(key)
  
  
  def findValuesFor(keyWord: String): Option[WordMappingValueSet] = 
    wordMappings.get(keyWord)
  
  
  def findPresentableQuizItem(numCorrectAnswersInARowDesired: Int,
      diffInPromptNumMinimum: Int, currentPromptNumber: Int): 
      Option[QuizItemViewWithOptions] =
    wordMappings.iterator.map(entry => 
        findPresentableQuizItem(entry._1, entry._2, numCorrectAnswersInARowDesired,
            diffInPromptNumMinimum, currentPromptNumber)).find(_.isDefined).getOrElse(None)

  
  def findPresentableQuizItem(key: String, wordMappingValues: WordMappingValueSet,
      numCorrectAnswersInARowDesired: Int, diffInPromptNumMinimum: Int, 
      currentPromptNumber: Int): Option[QuizItemViewWithOptions] = {

    val wordMappingValueOpt = wordMappingValues.findPresentableWordMappingValue(
        numCorrectAnswersInARowDesired, diffInPromptNumMinimum, currentPromptNumber) 
    wordMappingValueOpt match {
      case Some(wordMappingValue) => Some(quizItemWithOptions(key, 
          wordMappingValues, wordMappingValue))
      case _ => None
    }
  }
      
  private def quizItemWithOptions(key: String, wordMappingValues: WordMappingValueSet, 
      wordMappingValueCorrect: WordMappingValue): QuizItemViewWithOptions = {
    val numCorrectAnswers = wordMappingValueCorrect.numCorrectAnswersInARow
    val falseAnswers = makeFalseAnswers(key, wordMappingValues, 
        wordMappingValueCorrect, numCorrectAnswers)  
    new QuizItemViewWithOptions(key, wordMappingValueCorrect, 
        keyType, valueType, falseAnswers, numCorrectAnswers)
  }

  def makeFalseAnswers(key: String, wordMappingCorrectValues: WordMappingValueSet, 
      wordMappingValueCorrect: WordMappingValue,
      numCorrectAnswers: Int): Set[String] = {
    
    var falseAnswers = new HashSet[String]
    val numFalseAnswersRequired = 2
    
    /*
     * If the user has already been having success with this word, first try to
     * fill the falseAnswers with similar-looking words.
     */
    if (numCorrectAnswers >= 2)
      falseAnswers ++ makeFalseSimilarAnswers(wordMappingCorrectValues,
          wordMappingValueCorrect, numCorrectAnswers, numFalseAnswersRequired)
      
    // try again to fill the falseAnswers
    var totalTries = 20 // to stop any infinite loop
    while (falseAnswers.size < numFalseAnswersRequired && totalTries > 0) {
      totalTries = totalTries - 1
      val randomAnswer = findRandomWordValue(allWordMappingValues)
      if (!wordMappingCorrectValues.containsValue(randomAnswer))
         falseAnswers += randomAnswer
    }
    while (falseAnswers.size < numFalseAnswersRequired)
      falseAnswers += ""
    falseAnswers
  }
  
  def makeFalseSimilarAnswers(wordMappingCorrectValues: WordMappingValueSet,
      wordMappingValueCorrect: WordMappingValue, 
      numCorrectAnswersSoFar: Int, numFalseAnswersRequired: Int): Set[String] = {
    var falseAnswers = new HashSet[String]
    var totalTries = 20 // to stop any infinite loop
    
    while (falseAnswers.size < numFalseAnswersRequired && totalTries > 0) {
      totalTries = totalTries - 1
      val correctAnswer = wordMappingValueCorrect.value
      
      val similarityFunction = 
          if (numCorrectAnswersSoFar % 2 == 0) hasSameStart else hasSameEnd
      val randomAnswer = findSimilarValue(correctAnswer, similarityFunction)
      
      /* 
       * Avoid taking a value from the same wordMapping as the correct answer
       * E.g. if the correct answer for "full" is "voll" don't show "satt" as an option
       */
      if (!wordMappingCorrectValues.containsValue(randomAnswer))
        falseAnswers += randomAnswer
    }
    falseAnswers
  }  
  
  def hasSameStart = (wmv: WordMappingValue, value: String) => wmv.hasSameStart(value)
  def hasSameEnd = (wmv: WordMappingValue, value: String) => wmv.hasSameEnd(value)
  
  private def findSimilarValue(value: String, 
      similarityFunction: (WordMappingValue, String) => (Int => Boolean)): String = {
    
    val wordMappingValues = allWordMappingValues
      
    var similarWords = wordMappingValues.filter(
        wmv => similarityFunction(wmv, value)(5))
        
    // we need enough words to make a good random selection
    for (numOfLetters <- 4 to 0)
      if (similarWords.length < 5) 
        similarWords = wordMappingValues.filter(
            wmv => similarityFunction(wmv, value)(numOfLetters))
    
    val wordSelectionPool = 
      if (!similarWords.isEmpty) similarWords else allWordMappingValues 
    findRandomWordValue(wordSelectionPool)
  }
  
  def findRandomWordValue(wordMappingValues: ArrayBuffer[WordMappingValue]): String = {
    val randomIndex = Random.nextInt(wordMappingValues.length)
    wordMappingValues(randomIndex).value
  } 
}


object WordMappingGroup {
  def fromXML(node: xml.Node): WordMappingGroup = {
	new WordMappingGroup(keyType = (node \ "@keyType").text, 
	    valueType = (node \ "@valueType").text) {  
	  for (val wordMappingXml <- node \\ "wordMapping") {
	    val wmvs = WordMappingValueSet.fromXML(wordMappingXml)
	    addWordMapping((wordMappingXml \ "@key").text, Some(wmvs))
	  }
	}  
  }  
}