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
import scala.collection.immutable._
import scala.util.Random

import com.oranda.libanius.util.Util

case class WordMappingGroupReadWrite(override val keyType: String, override val valueType: String, 
      wordMappings: ListMap[String, WordMappingValueSet] = ListMap()) 
    extends WordMappingGroup(keyType, valueType) {
  // keyType example: "English word"
  // valueType example: "German word"
  
  def thisUpdated(newWordMappings: ListMap[String, WordMappingValueSet]) =
    WordMappingGroupReadWrite(keyType, valueType, newWordMappings)
    
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

  def numValues = wordMappings.values.iterator.foldLeft(0)(_ + _.size)
  
  def numItemsAndCorrectAnswers = {
    /*
     * The functional version (commented) is about 50% slower than the imperative version
     *  
     * wordMappings.values.iterator.foldLeft(Pair(0, 0))((acc, value) =>
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
  // def allWordMappingValues = combineValueSets(wordMappings.values)          
  
  /*
  def addWordMapping(keyValuePairOpt: Option[Pair[String, String]]): Unit =
    keyValuePairOpt.foreach { keyValuePair =>
      addWordMapping(keyValuePair._1, keyValuePair._2)
    }
  */
  
  def addWordMapping(key: String, value: String): WordMappingGroupReadWrite = {
    if (!key.isEmpty && !value.isEmpty && key.toLowerCase != value.toLowerCase)
      addWordMappingToEnd(key, value)
    else 
      this
  }
  
  private def updatedWordMappingValueSet(key: String, value: String): WordMappingValueSet = {
    val wmvsOld = findValuesFor(key)
    val wmvNew = WordMappingValue(value)
    wmvsOld match {
      case Some(wmvsOld) => wmvsOld.addValueToFront(wmvNew)
      case _ => new WordMappingValueSet(List(wmvNew))      
    }   
  }
  
  protected def addWordMappingToEnd(key: String, value: String): WordMappingGroupReadWrite = {
    val wmvsNew = updatedWordMappingValueSet(key, value)
    val wordMappingsNew = wordMappings + Pair(key, wmvsNew)
    thisUpdated(wordMappingsNew)
  }
  
  def addWordMappingToFront(key: String, value: String): WordMappingGroupReadWrite = {
    val wmvsNew = updatedWordMappingValueSet(key, value)
    val wordMappingsNew = ListMap[String, WordMappingValueSet](key -> wmvsNew) ++ 
        wordMappings.filterNot(_._1 == key) 
    thisUpdated(wordMappingsNew)
  }
  
  def addWordMapping(key: String, wordMappingValueSetOpt: Option[WordMappingValueSet]): 
      WordMappingGroupReadWrite =        
    if (wordMappingValueSetOpt.isDefined) addWordMapping(wordMappings, key, wordMappingValueSetOpt.get)
    else this
  
  def addWordMapping(key: String, wordMappingValueSet: WordMappingValueSet): 
      WordMappingGroupReadWrite = addWordMapping(wordMappings, key, wordMappingValueSet)
    
  def addWordMapping(wordMappings: ListMap[String, WordMappingValueSet], 
      key: String, wordMappingValueSet: WordMappingValueSet) = 
    thisUpdated(wordMappings + Pair(key, wordMappingValueSet))
  
  def removeWordMapping(key: String) = thisUpdated(wordMappings.filter(_._1 != key))

  def removeWordMappingValue(keyWord: String, wordMappingValue: WordMappingValue): 
      (WordMappingGroupReadWrite, Boolean) =
    wordMappings.get(keyWord) match {
      case Some(wmvs) => (addWordMapping(keyWord, wmvs.removeValue(wordMappingValue)), true)
      case _ => (this, false)
    }
  
  def findValuesFor(keyWord: String): Option[WordMappingValueSet] = 
    wordMappings.get(keyWord)
  
  def findPresentableQuizItem(currentPromptNumber: Int): 
      Option[QuizItemViewWithOptions] = 
    wordMappings.iterator.map(entry => 
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
    wordMappings.iterator.map(entry => 
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
    
    var falseAnswers = new ListSet[String]
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
    
    // final try to fill false answers: use dummy data
    if (falseAnswers.isEmpty) falseAnswers += ""
    while (falseAnswers.size < numFalseAnswersRequired)
      falseAnswers += (falseAnswers.last + " ")
 
    falseAnswers
  }
  
  def makeFalseSimilarAnswers(wordMappingCorrectValues: WordMappingValueSet,
      correctValue: WordMappingValue, numCorrectAnswersSoFar: Int, 
      numFalseAnswersRequired: Int): Set[String] = {
    var similarWords = new HashSet[String]
    val similarityFunction = if (numCorrectAnswersSoFar % 2 == 1) hasSameStart else hasSameEnd
    var numValueSetsSearched = 0
    wordMappings.values.iterator.takeWhile(
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
  
  def randomValues(sliceSize: Int) = 
    WordMappingValueSet.combineValueSets(randomSliceOfValueSets(sliceSize))
  
  def randomSliceOfValueSets(sliceSize: Int): Iterable[WordMappingValueSet] =
    if (sliceSize >= size) wordMappings.values.toList
    else {
      val randomStart = Random.nextInt(size - sliceSize)
      wordMappings.values.slice(randomStart, randomStart + sliceSize).toList
    }
  
  def merge(otherWmg: Option[WordMappingGroupReadWrite]): WordMappingGroupReadWrite =
    otherWmg match {
      case Some(otherWmg) => merge(otherWmg)
      case _ => this
    }
  
  def merge(otherWmg: WordMappingGroupReadWrite): WordMappingGroupReadWrite = {
    val wordMappingsCombined = otherWmg.wordMappings.foldLeft(wordMappings) { 
      (acc, kvPair) => wordMappings + Pair(kvPair._1, kvPair._2)
    }
    thisUpdated(wordMappingsCombined)
  }
  
  def keyBeginningWith(keyStart: String) = 
    wordMappings.keys.find(_.startsWith(keyStart)) 
  
  def hasKey(key: String): Boolean = wordMappings.isDefinedAt(key)
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
  def fromCustomFormat(str: String): WordMappingGroupReadWrite = {
    
    // Initial parsing is performance-critical, so this part is done in a mutable Java map.
    //var wordMappings = new java.util.LinkedHashMap[String, WordMappingValueSet]()
    var wordMappings = ListMap[String, WordMappingValueSet]()
    
    splitterLineBreak.setString(str)
    splitterLineBreak.next // skip the first line, which has already been parsed
      
    while (splitterLineBreak.hasNext) {
      splitterKeyValue.setString(splitterLineBreak.next)
        
      if (splitterKeyValue.hasNext) {
        val strKey = splitterKeyValue.next
        if (splitterKeyValue.hasNext) {
          val strValues = splitterKeyValue.next
          wordMappings += Pair(strKey, WordMappingValueSet.fromCustomFormat(strValues))
        }
      }
    }
    
    //val list = wordMappings.toList
    
    //val listMap: collection.immutable.ListMap[String, WordMappingValueSet] = 
    //    list.map{ e => (e._1, e._2) } (collection.breakOut)
  
    // Now use the persistent data structure.
    var wmg = new WordMappingGroupReadWrite(keyType = WordMappingGroup.parseKeyType(str),
        valueType = WordMappingGroup.parseValueType(str), wordMappings /*ListMap(wordMappings.toList:_*)*/)
    
    wmg
  }

  /*  
  def fromXML(node: xml.Node): WordMappingGroupReadWrite = {
	new WordMappingGroupReadWrite(keyType = (node \ "@keyType").text, 
	    valueType = (node \ "@valueType").text) {  
	  for (wordMappingXml <- node \\ "wordMapping") {
	    val wmvs = WordMappingValueSet.fromXML(wordMappingXml)
	    addWordMapping((wordMappingXml \ "@key").text, Some(wmvs))
	  }
	}  
  }*/
}