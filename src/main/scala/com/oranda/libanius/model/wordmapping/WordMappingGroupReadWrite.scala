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

import com.oranda.libanius.util.{Platform, StringUtil, Util}
import com.oranda.libanius.SaveData
import com.oranda.libanius.model.UserAnswer

case class WordMappingGroupReadWrite(override val header: QuizGroupHeader,
      wordMappings: Stream[Pair[String, WordMappingValueSetWrapperBase]] = Stream.empty,
      currentPromptNumber: Int = 0,
      currentSearchRange: Range = 0 until WordMappingGroupReadWrite.rangeSize)
    extends WordMappingGroup(header) with Platform {

  def keyType = header.keyType     // example: "English word"
  def valueType = header.valueType // example: "German word"

  // workaround: compiler does not accept import
  lazy val rangeSize = WordMappingGroupReadWrite.rangeSize

  def thisUpdated(newWordMappings: Stream[Pair[String, WordMappingValueSetWrapperBase]]) =
    WordMappingGroupReadWrite(header, newWordMappings, currentPromptNumber, currentSearchRange)

  def updatedPromptNumber =
    WordMappingGroupReadWrite(header, wordMappings, currentPromptNumber + 1, currentSearchRange)

  def updatedSearchRange =
    WordMappingGroupReadWrite(header, wordMappings, currentPromptNumber, rangeForNextSearch)

  def wordMappingValueSets = wordMappings.view.map(_._2)
  def wordMappingKeys = wordMappings.view.map(_._1)
    
  /*
   * Example of custom format:
   * 
   * wordMappingGroup keyType="English word" valueType="German word" currentPromptNumber="0"
   *    against|wider
   *    entertain|unterhalten
   */
  def toCustomFormat(strBuilder: StringBuilder) = {
    header.toCustomFormat(strBuilder).append(" currentPromptNumber=\"").
        append(currentPromptNumber).append("\"")
    val iter = wordMappings.iterator
    while (iter.hasNext) {
      val wordMapping = iter.next
      strBuilder.append('\n').append(wordMapping._1).append('|')
      wordMapping._2.toCustomFormat(strBuilder)
    }      
    strBuilder
  }

  def getSaveData: SaveData = {
    val serialized = toCustomFormat(new StringBuilder())
    val fileName = header.makeFileName
    SaveData(fileName, serialized.toString)
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
  
  def addWordMapping(key: String, value: String): WordMappingGroupReadWrite =
    if (!key.isEmpty && !value.isEmpty && key.toLowerCase != value.toLowerCase)
      addWordMappingToEnd(key, value)
    else 
      this

  private def updatedWordMappingValueSet(key: String, value: String):
      WordMappingValueSetWrapperBase =
    updatedWordMappingValueSet(key, WordMappingValue(value))

  def updateWithUserAnswer(key: String, wmvs: WordMappingValueSet, wmv: WordMappingValue,
      userAnswer: UserAnswer): WordMappingGroupReadWrite = {
    val wmvUpdated = wmv.addUserAnswer(userAnswer)
    updateWordMappingValue(key, wmvs, wmvUpdated)
  }

  def updateWordMappingValue(key: String, wmvsOld: WordMappingValueSet, wmv: WordMappingValue):
      WordMappingGroupReadWrite = {
    val wmvsNew = WordMappingValueSetWrapper(wmvsOld.replaceWmv(wmv))
    addWordMappingToFront(key, wmvsNew)
  }

  private def updatedWordMappingValueSet(key: String, wmvNew: WordMappingValue):
      WordMappingValueSetWrapperBase = {
    val wmvsOld = findValueSetFor(key)
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
    thisUpdated(Pair(key, wmvsNew) +: wordMappings.filterNot(_._1 == key))
  }

  protected def addWordMappingToFront(key: String,
      wordMappingValueSet: WordMappingValueSetWrapperBase): WordMappingGroupReadWrite =
    addWordMappingToFront(wordMappings, key, wordMappingValueSet)

  protected def addWordMappingToFront(
      wordMappings: Stream[Pair[String, WordMappingValueSetWrapperBase]],
      key: String, wmvsNew: WordMappingValueSetWrapperBase): WordMappingGroupReadWrite =
    thisUpdated(Pair(key, wmvsNew) +: wordMappings.filterNot(_._1 == key))

  def removeWordMapping(key: String) = thisUpdated(wordMappings.filter(_._1 != key))

  def removeWordMappingValue(keyWord: String, wordMappingValue: WordMappingValue): 
      (WordMappingGroupReadWrite, Boolean) =
    findValueSetFor(keyWord) match {
      case Some(wmvs) =>
        val wmvsNew = removeValue(wmvs, wordMappingValue)
        (addWordMappingToEnd(keyWord, wmvsNew), true)
      case _ => (this, false)
    }
  
  private def removeValue(wmvs: WordMappingValueSetWrapperBase, wmv: WordMappingValue) =
    WordMappingValueSetWrapper(wmvs.removeValue(wmv))
  
  // Low usage expected. Slow because we are not using a Map for wordMappings. 
  def findValueSetFor(keyWord: String): Option[WordMappingValueSetWrapperBase] =
    wordMappings.find(_._1 == keyWord).map(_._2)
  
  def findPresentableQuizItem: Option[QuizItemViewWithOptions] = {
    log("currentSearchRange: " + currentSearchRange.start)
    val wmSlice = wordMappings.slice(currentSearchRange.start, currentSearchRange.end)
    val quizItem =
      (for {
        wm <- wmSlice.toStream
        quizItem <- findPresentableQuizItem(wm._1, wm._2, currentPromptNumber)
      } yield quizItem).headOption
    log("found quiz item " + quizItem)
    quizItem
  }
  
  def rangeForNextSearch: Range = {
    if (currentSearchRange.end > wordMappings.size) 0 until rangeSize
    else currentSearchRange.start + rangeSize until currentSearchRange.end + rangeSize
  }

  private def findPresentableQuizItem(key: String, wordMappingValues: WordMappingValueSet, 
      currentPromptNumber: Int): Option[QuizItemViewWithOptions] = {
    //log("findPresentableQuizItem: key=" + key + ", currentPromptNumber=" +
    //    currentPromptNumber)
    val wordMappingValue = wordMappingValues.findPresentableWordMappingValue(currentPromptNumber)
    wordMappingValue.map { wordMappingValue =>
      Util.stopwatch(quizItemWithOptions(key, wordMappingValues, wordMappingValue),
          "quizItemWithOptions for " + wordMappingValue)
    }
  }
  
  def findAnyUnfinishedQuizItem: Option[QuizItemViewWithOptions] = {
    log("findAnyUnfinishedQuizItem " + header)
    wordMappings.iterator.map(entry => 
        findAnyUnfinishedQuizItem(entry._1,  entry._2)).
        find(_.isDefined).getOrElse(None)
  }

  private def findAnyUnfinishedQuizItem(key: String, 
      wordMappingValues: WordMappingValueSet): Option[QuizItemViewWithOptions] =
    wordMappingValues.findAnyUnfinishedWordMappingValue.map(
        quizItemWithOptions(key, wordMappingValues, _))
    
  private def quizItemWithOptions(key: String, wmvs: WordMappingValueSet,
      wordMappingValueCorrect: WordMappingValue): QuizItemViewWithOptions = {
    val numCorrectAnswers = wordMappingValueCorrect.numCorrectAnswersInARow
    val falseAnswers = makeFalseAnswers(key, wmvs,
        wordMappingValueCorrect, numCorrectAnswers)
    new QuizItemViewWithOptions(key, wmvs, wordMappingValueCorrect,
        currentPromptNumber, header, falseAnswers, numCorrectAnswers)
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
      randomAnswer.foreach( randomAnswer =>
        if (!wordMappingCorrectValues.containsValue(randomAnswer))
          falseAnswers += randomAnswer
      )
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
                if (wmvs.wmvs ne wordMappingCorrectValues) {
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
  
  def findRandomWordValue(wordMappingValues: Seq[WordMappingValue]): Option[String] = {
    if (wordMappingValues.isEmpty)
      None
    else {
      val randomIndex = Random.nextInt(wordMappingValues.length)
      Some(wordMappingValues(randomIndex).value)
    }
  } 
  
  def randomValues(sliceSize: Int) =  {
    val combinedValueSets = WordMappingValueSet.combineValueSets(randomSliceOfValueSets(sliceSize))
    if (combinedValueSets.isEmpty)
      log("ERROR: randomValues found nothing")
    combinedValueSets
  }
  
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


object WordMappingGroupReadWrite extends Platform {
  
  val rangeSize = 200

  /*
   * Example:
   * 
   * wordMappingGroup keyType="English word" valueType="German word" currentPromptNumber="0"
   *    against|wider
   *    entertain|unterhalten
   */
  def fromCustomFormat(str: String): WordMappingGroupReadWrite = {

    val splitterLineBreak = WordMappingGroup.splitterLineBreak
    val splitterKeyValue = WordMappingGroup.splitterKeyValue

    // TODO: write directly to the Stream not a ListBuffer
    val wordMappingsMutable = new ListBuffer[Pair[String, WordMappingValueSetWrapperBase]]()

    try {
      splitterLineBreak.setString(str)
      splitterLineBreak.next // skip the first line, which has already been parsed

      while (splitterLineBreak.hasNext) {
        val strKeyValue = splitterLineBreak.next
        splitterKeyValue.setString(strKeyValue)

        if (splitterKeyValue.hasNext) {

          try {
            val strKey = splitterKeyValue.next

            if (splitterKeyValue.hasNext) {
              val strValues = splitterKeyValue.next
              wordMappingsMutable += Pair(strKey, WordMappingValueSetLazyProxy(strValues))
            }
          } catch {
            case e: Exception => log("could not parse key-value string: " + strKeyValue)
          }
        }
      }
    } catch {
      case e: Exception => log("could not parse wmg with str " + str.take(100) + "..." + str.takeRight(100))
    }

    val wordMappingsStream = wordMappingsMutable.toStream
  
    // Now use the persistent data structure.
    new WordMappingGroupReadWrite(QuizGroupHeader(str),
        wordMappings = wordMappingsStream,
        currentPromptNumber = WordMappingGroupReadWrite.parseCurrentPromptNumber(str))
  }

  def parseCurrentPromptNumber(str: String): Int =
    try {
      StringUtil.parseValue(str, "currentPromptNumber=\"", "\"").toInt
    } catch {
      case e: Exception => log("Could not parse prompt number from " + str)
                           0
    }
}