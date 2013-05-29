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

import scala.collection.JavaConversions.mapAsScalaMap
import scala.collection.immutable.HashSet
import scala.collection.mutable.LinkedHashMap
import scala.collection.mutable
import scala.util.Random

import com.oranda.libanius.util.Util

case class WordMappingGroupReadWrite(_keyType: String, _valueType: String) 
    extends WordMappingGroup(_keyType, _valueType) {
  // keyType example: "English word"
  // valueType example: "German word"
  
  // When populating, the java.util map is faster than the mutable Scala map.
  private val wordMappings = new java.util.LinkedHashMap[String, WordMappingValueSet]
  
  // ... but apart from that we want a Scala version of the map so as to use Scala syntax
  private def wordMappingsAsScala: mutable.Map[String, WordMappingValueSet] = 
    wordMappings
  
  def toXML =
    <wordMappingGroup keyType={keyType} valueType={valueType}>
      {wordMappings map (keyValue => 
          <wordMapping key={keyValue._1}>{keyValue._2.toXML}</wordMapping> ) }
    </wordMappingGroup>  
  
  /*
   * Example of custom format:
   * 
   * wordMappingGroup keyType="English word" valueType="German word"
   *    against|wider
   *    entertain|unterhalten
   */
  def toCustomFormat(strBuilder: StringBuilder) = {
    strBuilder.append("wordMappingGroup keyType=\"").append(keyType).
        append("\" valueType=\"").append(valueType).append("\"")
    val iter = wordMappings.iterator
    while (iter.hasNext) {
      val wordMapping = iter.next
      strBuilder.append('\n').append(wordMapping._1).append('|')
      wordMapping._2.toCustomFormat(strBuilder)
    }      
    strBuilder
  }

  def size = wordMappings.size  
  def numKeyWords = wordMappings.size

  def numValues = wordMappingsAsScala.values.iterator.foldLeft(0)(_ + _.size)
  
  def numItemsAndCorrectAnswers = {
    /*
     * The functional version (commented) is about 50% slower than the imperative version
     *  
     * wordMappingsAsScala.values.iterator.foldLeft(Pair(0, 0))((acc, value) =>
     *    (acc._1 + value.numItemsAndCorrectAnswers._1, 
     *     acc._2 + value.numItemsAndCorrectAnswers._2))
     */
    var numItems = 0
    var numCorrectAnswers = 0
    val wordMappingsIter = wordMappings.iterator
    while (wordMappingsIter.hasNext) {
      val wmvs = wordMappingsIter.next._2
      val _numItemsAndCorrectAnswers = wmvs.numItemsAndCorrectAnswers
      numItems += _numItemsAndCorrectAnswers._1
      numCorrectAnswers +=_numItemsAndCorrectAnswers._2
    }
    Pair(numItems, numCorrectAnswers)
  }
    
         
  def contains(wordMapping: String): Boolean = wordMappings.contains(wordMapping)

  // probably too slow to be useful  (and not a lazy val because values can be deleted)
  // def allWordMappingValues = combineValueSets(wordMappingsAsScala.values)          
  
  def addWordMapping(keyValuePairOpt: Option[Pair[String, String]]): Unit =
    keyValuePairOpt.foreach { keyValuePair =>
      addWordMapping(keyValuePair._1, keyValuePair._2)
    }
    
  def addWordMapping(key: String, value: String) {
    if (!key.isEmpty && !value.isEmpty && key.toLowerCase != value.toLowerCase)
      addWordMappingCore(key, value)
  }
  
  protected def addWordMappingCore(key: String, value: String) {
    val existingValuesOpt = wordMappingsAsScala.get(key)
    val existingValues = existingValuesOpt match {
        case Some(existingValues) => existingValues
        case None => val wmvs = new WordMappingValueSet()
          wordMappings.put(key, wmvs)
          wmvs
    }          
    existingValues.addValue(new WordMappingValue(value))
  }
  
  def addWordMappingToFront(keyWord: String, value: String) {
  
    implicit def prepend[K, V](map: mutable.Map[K, V]) = new {
      def prepend(kv: (K, V)) {
        // Warning: val copy = map.toMap causes a 2nd prepend to fail
        val copy = new mutable.LinkedHashMap[K, V]
        copy ++= map
        map.clear
        map += kv
        map ++= copy
      }
    }
    
    val newWordMappings = new java.util.LinkedHashMap[String, WordMappingValueSet]
    val wmvsOpt = findValuesFor(keyWord)
    var wmvsNew = wmvsOpt match {
      case Some(wmvs) => wmvsOpt.get.filterOut(value)
      case _ => new WordMappingValueSet
    }
    wmvsNew.addValue(new WordMappingValue(value))
    wordMappingsAsScala.prepend((keyWord, wmvsNew))
  }
  
  def addWordMapping(key: String, wordMappingValueSetOpt: Option[WordMappingValueSet]) =
    wordMappingValueSetOpt.foreach(wordMappings.put(key, _))
  
  def addWordMapping(key: String, wordMappingValueSet: WordMappingValueSet) =
    wordMappings.put(key, wordMappingValueSet)
  
  def removeWordMapping(key: String): Option[WordMappingValueSet] = 
    wordMappingsAsScala.remove(key)

  def removeWordMappingValue(keyWord: String, 
      wordMappingValue: WordMappingValue): Boolean = {
    val wmvs = wordMappings.get(keyWord)
    wmvs != null && wmvs.removeValue(wordMappingValue)
  }
  
  def findValuesFor(keyWord: String): Option[WordMappingValueSet] = 
    wordMappingsAsScala.get(keyWord)
  
  
  def findPresentableQuizItem(currentPromptNumber: Int): 
      Option[QuizItemViewWithOptions] = 
    wordMappingsAsScala.iterator.map(entry => 
        findPresentableQuizItem(entry._1, entry._2, currentPromptNumber)).
        find(_.isDefined).getOrElse(findAnyUnfinishedQuizItem)

  private def findPresentableQuizItem(key: String, 
      wordMappingValues: WordMappingValueSet, currentPromptNumber: Int): 
      Option[QuizItemViewWithOptions] = {
    val wordMappingValueOpt = wordMappingValues.findPresentableWordMappingValue(
        currentPromptNumber)
    wordMappingValueOpt.map { wordMappingValue =>
      Util.stopwatch(quizItemWithOptions(key, wordMappingValues, wordMappingValue), 
      "quizItemWithOptions for " + wordMappingValue) 
    }
  }
  
  def findAnyUnfinishedQuizItem: Option[QuizItemViewWithOptions] =
    wordMappingsAsScala.iterator.map(entry => 
        findAnyUnfinishedQuizItem(entry._1,  entry._2)).
        find(_.isDefined).getOrElse(None)

  private def findAnyUnfinishedQuizItem(key: String, 
      wordMappingValues: WordMappingValueSet): Option[QuizItemViewWithOptions] =
    wordMappingValues.findAnyUnfinishedWordMappingValue.map(
        quizItemWithOptions(key, wordMappingValues, _))
    
  private def quizItemWithOptions(key: String, wordMappingValues: WordMappingValueSet, 
      wordMappingValueCorrect: WordMappingValue): QuizItemViewWithOptions = {
    val numCorrectAnswers = wordMappingValueCorrect.numCorrectAnswersInARow
    val falseAnswers = makeFalseAnswers(key, wordMappingValues, 
        wordMappingValueCorrect, numCorrectAnswers)
    new QuizItemViewWithOptions(key, wordMappingValueCorrect, 
        keyType, valueType, falseAnswers, numCorrectAnswers)
  }

  def makeFalseAnswers(key: String, wordMappingCorrectValues: WordMappingValueSet, 
      wordMappingValueCorrect: WordMappingValue, numCorrectAnswersSoFar: Int): 
      Set[String] = {
    
    var falseAnswers = new HashSet[String]
    val numFalseAnswersRequired = 2
    
    /*
     * If the user has already been having success with this word, first try to
     * fill the falseAnswers with similar-looking words.
     */
    if (numCorrectAnswersSoFar >= 1) 
      falseAnswers ++= Util.stopwatch(makeFalseSimilarAnswers(wordMappingCorrectValues,
          wordMappingValueCorrect, numCorrectAnswersSoFar, numFalseAnswersRequired), 
          "makeFalseSimilarAnswers")
    
    // try again to fill the falseAnswers
    var totalTries = 20 // to stop any infinite loop
    while (falseAnswers.size < numFalseAnswersRequired && totalTries > 0) {
      totalTries = totalTries - 1
      val randomAnswer = findRandomWordValue(randomValues(100)) 
      if (!wordMappingCorrectValues.containsValue(randomAnswer))
         falseAnswers += randomAnswer
    }
    while (falseAnswers.size < numFalseAnswersRequired)
      falseAnswers += ""
    falseAnswers
  }
  
  def makeFalseSimilarAnswers(wordMappingCorrectValues: WordMappingValueSet,
      correctValue: WordMappingValue, numCorrectAnswersSoFar: Int, 
      numFalseAnswersRequired: Int): Set[String] = {
    var similarWords = new HashSet[String]
    val similarityFunction = 
          if (numCorrectAnswersSoFar % 2 == 1) hasSameStart else hasSameEnd
    var numValueSetsSearched = 0
    wordMappingsAsScala.values.iterator.takeWhile(
        _ => similarWords.size < numFalseAnswersRequired).
            foreach(
              wmvs => {
                numValueSetsSearched = numValueSetsSearched + 1
                // Avoid selecting values belonging to the "correct" value set
                if (wmvs ne wordMappingCorrectValues) {
                  val numSimilarLetters = 2
                  wmvs.values.foreach { wmv =>
                    if (similarWords.size < numFalseAnswersRequired &&
                        similarityFunction(wmv, correctValue.value)(numSimilarLetters))
                      similarWords += wmv.value
                  }
                }
            })
    similarWords
  }  
  
  def hasSameStart = (wmv: WordMappingValue, value: String) => wmv.hasSameStart(value)
  def hasSameEnd = (wmv: WordMappingValue, value: String) => wmv.hasSameEnd(value)
  
  def findRandomWordValue(wordMappingValues: Seq[WordMappingValue]): String = {
    val randomIndex = Random.nextInt(wordMappingValues.length)
    wordMappingValues(randomIndex).value
  } 
  
  def randomValues(sliceSize: Int) = WordMappingValueSet.combineValueSets(
      randomSliceOfValueSets(sliceSize))
  
  def randomSliceOfValueSets(sliceSize: Int): Iterable[WordMappingValueSet] = {
    if (sliceSize >= size)
      wordMappingsAsScala.values
    else {
      val randomStart = Random.nextInt(size - sliceSize)
      wordMappingsAsScala.values.slice(randomStart, randomStart + sliceSize).
          toArray[WordMappingValueSet]
    }
  }
  
  def merge(otherWmg: WordMappingGroupReadWrite) {
    otherWmg.wordMappingsAsScala.foreach { kvPair => 
      addWordMapping(kvPair._1, kvPair._2) 
    }
  }
  
  def keyBeginningWith(keyStart: String) = 
    wordMappings.keys.find(_.startsWith(keyStart)) 
  
  def hasKey(key: String): Boolean = wordMappings.containsKey(key)
}


object WordMappingGroupReadWrite {
  
  val splitterLineBreak = WordMappingGroup.splitterLineBreak
  val splitterKeyValue = WordMappingGroup.splitterKeyValue
  
  /*
   * Example:
   * 
   * wordMappingGroup keyType="English word" valueType="German word"
   *    against|wider
   *    entertain|unterhalten
   */
  def fromCustomFormat(str: String): WordMappingGroupReadWrite =
    new WordMappingGroupReadWrite(_keyType = WordMappingGroup.parseKeyType(str),
        _valueType = WordMappingGroup.parseValueType(str)) {
      
      splitterLineBreak.setString(str)
      splitterLineBreak.next // skip the first line, which has already been parsed
      
      while (splitterLineBreak.hasNext) {
        splitterKeyValue.setString(splitterLineBreak.next)
        
        if (splitterKeyValue.hasNext) {
          val strKey = splitterKeyValue.next
          if (splitterKeyValue.hasNext) {
            val strValues = splitterKeyValue.next
            addWordMapping(strKey, WordMappingValueSet.fromCustomFormat(strValues))
          }
        }
      }
    }
    
  def fromXML(node: xml.Node): WordMappingGroupReadWrite = {
	new WordMappingGroupReadWrite(_keyType = (node \ "@keyType").text, 
	    _valueType = (node \ "@valueType").text) {  
	  for (wordMappingXml <- node \\ "wordMapping") {
	    val wmvs = WordMappingValueSet.fromXML(wordMappingXml)
	    addWordMapping((wordMappingXml \ "@key").text, Some(wmvs))
	  }
	}  
  }
}