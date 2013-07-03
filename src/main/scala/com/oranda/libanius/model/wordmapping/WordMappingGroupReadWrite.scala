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

import scala.collection.mutable.ListBuffer
import scala.collection.immutable._
import scala.util.Random

import com.oranda.libanius.util.Util
import WordMappingGroupReadWrite._

case class WordMappingGroupReadWrite(override val keyType: String, override val valueType: String, 
      wordMappings: Stream[Pair[String, WordMappingValueSetWrapperBase]] = Stream.empty) 
    extends WordMappingGroup(keyType, valueType) {
  // keyType example: "English word"
  // valueType example: "German word"
  
  // This mutable range is used by the findPresentableQuizItem function to improve performance
  var currentItemSearchRange = 0 until rangeSize
  
  def thisUpdated(newWordMappings: Stream[Pair[String, WordMappingValueSetWrapperBase]]) =
    WordMappingGroupReadWrite(keyType, valueType, newWordMappings)

  
  def wordMappingValueSets = wordMappings.view.map(_._2)
  def wordMappingKeys = wordMappings.view.map(_._1)
    
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

  def numValues = wordMappingValueSets.iterator.foldLeft(0)(_ + _.size)
  
  def numItemsAndCorrectAnswers: Pair[Int, Int]  = {
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
    
         
  def contains(wordMapping: String): Boolean = wordMappingKeys.contains(wordMapping)

  // probably too slow to be useful  (and not a lazy val because values can be deleted)
  // def allWordMappingValues = combineValueSets(wordMappings.values)
  
  def addWordMapping(key: String, value: String): WordMappingGroupReadWrite = {
    if (!key.isEmpty && !value.isEmpty && key.toLowerCase != value.toLowerCase)
      addWordMappingToEnd(key, value)
    else 
      this
  }
  
  private def updatedWordMappingValueSet(key: String, value: String): 
      WordMappingValueSetWrapperBase = {
    val wmvsOld = findValuesFor(key)
    val wmvNew = WordMappingValue(value)
    WordMappingValueSetWrapper(
      wmvsOld match {
        case Some(wmvsOld) => wmvsOld.addValueToFront(wmvNew)
        case _ => WordMappingValueSet(List(wmvNew))      
      })
  }

  
  def addWordMapping(key: String, wordMappingValueSetOpt: Option[WordMappingValueSetWrapperBase]): 
      WordMappingGroupReadWrite =        
    if (wordMappingValueSetOpt.isDefined)
      addWordMappingToEnd(wordMappings, key, wordMappingValueSetOpt.get)
    else 
      this

  protected def addWordMappingToEnd(key: String, value: String): WordMappingGroupReadWrite = {
    val wmvsNew = updatedWordMappingValueSet(key, value)
    addWordMappingToEnd(key, wmvsNew)
  }

  protected def addWordMappingToEnd(key: String,
      wordMappingValueSet: WordMappingValueSetWrapperBase): WordMappingGroupReadWrite =
    addWordMappingToEnd(wordMappings, key, wordMappingValueSet)

  protected def addWordMappingToEnd(
      wordMappings: Stream[Pair[String, WordMappingValueSetWrapperBase]],
      key: String, wmvsNew: WordMappingValueSetWrapperBase): WordMappingGroupReadWrite =
    thisUpdated(wordMappings.filterNot(_._1 == key) :+ Pair(key, wmvsNew))


  def addWordMappingToFront(key: String, value: String): WordMappingGroupReadWrite = {
    val wmvsNew = updatedWordMappingValueSet(key, value)
    val wordMappingsNew = Pair(key, wmvsNew) +: wordMappings.filterNot(_._1 == key)
    thisUpdated(wordMappingsNew)
  }
  
  def removeWordMapping(key: String) = thisUpdated(wordMappings.filter(_._1 != key))

  def removeWordMappingValue(keyWord: String, wordMappingValue: WordMappingValue): 
      (WordMappingGroupReadWrite, Boolean) =
    findValuesFor(keyWord) match {
      case Some(wmvs) => (addWordMappingToEnd(keyWord, removeValue(wmvs, wordMappingValue)), true)
      case _ => (this, false)
    }
  
  private def removeValue(wmvs: WordMappingValueSetWrapperBase, wmv: WordMappingValue) =
    WordMappingValueSetWrapper(wmvs.removeValue(wmv))
  
  // Low usage expected. Slow because we are not using a Map for wordMappings. 
  def findValuesFor(keyWord: String): Option[WordMappingValueSetWrapperBase] = 
    wordMappings.find(_._1 == keyWord).map(_._2)
  
  def findPresentableQuizItem(currentPromptNumber: Int): Option[QuizItemViewWithOptions] = {
    //Util.log("Libanius", "currentItemSearchRange: " + currentItemSearchRange.start)
    val wmSlice = wordMappings.slice(currentItemSearchRange.start, currentItemSearchRange.end)
    moveRangeForNextSearch()
    wmSlice.iterator.map(entry => 
        findPresentableQuizItem(entry._1, entry._2, currentPromptNumber)).
        find(_.isDefined).getOrElse(findAnyUnfinishedQuizItem)
  }
  
  def moveRangeForNextSearch() {
    currentItemSearchRange = 
      if (currentItemSearchRange.end > wordMappings.size) 0 until rangeSize
      else currentItemSearchRange.start + rangeSize until currentItemSearchRange.end + rangeSize
  }

  private def findPresentableQuizItem(key: String, wordMappingValues: WordMappingValueSet, 
      currentPromptNumber: Int): Option[QuizItemViewWithOptions] = {
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
    wordMappingValueSets.iterator.takeWhile(
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
  
  def randomSliceOfValueSets(sliceSize: Int): Iterable[WordMappingValueSetWrapperBase] =
    if (sliceSize >= size) wordMappingValueSets.toList
    else {
      val randomStart = Random.nextInt(size - sliceSize)
      val slice = wordMappings.slice(randomStart, randomStart + sliceSize)
      val valueSetsSlice = slice.map(_._2)
      valueSetsSlice.toList
    }
  
  def merge(otherWmg: Option[WordMappingGroupReadWrite]): WordMappingGroupReadWrite =
    otherWmg match {
      case Some(otherWmg) => merge(otherWmg)
      case _ => this
    }
  
  def merge(otherWmg: WordMappingGroupReadWrite): WordMappingGroupReadWrite = {
    val wordMappingsCombined = otherWmg.wordMappings.foldLeft(wordMappings) { 
      (acc, kvPair) => wordMappings :+ Pair(kvPair._1, kvPair._2)
    }
    thisUpdated(wordMappingsCombined)
  }
  
  def hasKey(key: String): Boolean = wordMappingKeys.find(_ == key).isDefined
  def keyBeginningWith(keyStart: String) = wordMappingKeys.find(_.startsWith(keyStart)) 
  
}


object WordMappingGroupReadWrite {
  
  val rangeSize = 200
  
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
    
    // TODO: write directly to the Stream not a ListBuffer
    val wordMappingsMutable = new ListBuffer[Pair[String, WordMappingValueSetWrapperBase]]()
    
    splitterLineBreak.setString(str)
    splitterLineBreak.next // skip the first line, which has already been parsed
      
    while (splitterLineBreak.hasNext) {
      splitterKeyValue.setString(splitterLineBreak.next)
        
      if (splitterKeyValue.hasNext) {
        val strKey = splitterKeyValue.next
        if (splitterKeyValue.hasNext) {
          val strValues = splitterKeyValue.next
          wordMappingsMutable += Pair(strKey, WordMappingValueSetLazyProxy(strValues)) 
        }
      }
    }
    
    val wordMappings = wordMappingsMutable.toStream
  
    // Now use the persistent data structure.
    new WordMappingGroupReadWrite(keyType = WordMappingGroup.parseKeyType(str),
        valueType = WordMappingGroup.parseValueType(str), wordMappings)
  }

}