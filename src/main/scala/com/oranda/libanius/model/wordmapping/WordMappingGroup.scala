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
import scala.util.{Try, Random}

import com.oranda.libanius.util.{Platform, StringUtil, Util}
import com.oranda.libanius.SaveData
import com.oranda.libanius.model.{ModelComponent, UserAnswer}

case class WordMappingGroup(val header: QuizGroupHeader,
    wordMappings: Stream[WordMappingPair] = Stream.empty,
    currentPromptNumber: Int = 0,
    currentSearchRange: Range = 0 until WordMappingGroup.rangeSize)
  extends ModelComponent with Platform {

  def keyType = header.keyType     // example: "English word"
  def valueType = header.valueType // example: "German word"

  var dictionary = Dictionary()

  // workaround: compiler does not accept import
  lazy val rangeSize = WordMappingGroup.rangeSize

  def thisUpdated(newWordMappings: Stream[WordMappingPair]) =
    WordMappingGroup(header, newWordMappings, currentPromptNumber, currentSearchRange)

  def updatedPromptNumber =
    WordMappingGroup(header, wordMappings, currentPromptNumber + 1, currentSearchRange)

  def updatedSearchRange =
    WordMappingGroup(header, wordMappings, currentPromptNumber, rangeForNextSearch)

  def resetSearchRange =
    WordMappingGroup(header, wordMappings, currentPromptNumber, 0 until WordMappingGroup.rangeSize)

  def wordMappingKeys = wordMappings.view.map(_.key)
  def wordMappingValueSets = wordMappings.view.map(_.valueSet)

  def matches(wmgOther: WordMappingGroup): Boolean = header.matches(wmgOther.header)

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
      strBuilder.append('\n').append(wordMapping.key).append('|')
      wordMapping.valueSet.toCustomFormat(strBuilder)
    }
    strBuilder
  }

  def setDictionary(dict: Dictionary) {
    dictionary = dict
  }

  def getSaveData: SaveData = {
    val serialized = toCustomFormat(new StringBuilder())
    val fileName = header.makeWmgFileName
    SaveData(fileName, serialized.toString)
  }

  def size = wordMappings.size
  def numKeyWords = wordMappings.size

  def numValues = wordMappingValueSets.iterator.foldLeft(0)(_ + _.size)

  def numItemsAndCorrectAnswers: Pair[Int, Int]  = {
    /*
     * The functional version is about 50% slower than the imperative version
     *
     * wordMappings.values.iterator.foldLeft(Pair(0, 0))((acc, value) =>
     *    (acc._1 + value.numItemsAndCorrectAnswers._1,
     *     acc._2 + value.numItemsAndCorrectAnswers._2))
     */
    var numItems = 0
    var numCorrectAnswers = 0
    val wordMappingsIter = wordMappings.iterator
    while (wordMappingsIter.hasNext) {
      val wmvs = wordMappingsIter.next.valueSet
      val _numItemsAndCorrectAnswers = wmvs.numItemsAndCorrectAnswers
      numItems += _numItemsAndCorrectAnswers._1
      numCorrectAnswers +=_numItemsAndCorrectAnswers._2
    }
    Pair(numItems, numCorrectAnswers)
  }

  def contains(wordMapping: String): Boolean = wordMappingKeys.contains(wordMapping)

  // probably too slow to be useful  (and not a lazy val because values can be deleted)
  // def allWordMappingValues = combineValueSets(wordMappings.values)

  def addWordMapping(key: String, value: String): WordMappingGroup =
    if (!key.isEmpty && !value.isEmpty && key.toLowerCase != value.toLowerCase)
      addWordMappingToEnd(key, value)
    else
      this

  private def updatedWordMappingValueSet(key: String, value: String):
      WordMappingValueSetWrapperBase =
    updatedWordMappingValueSet(key, WordMappingValue(value))

  def updateWithUserAnswer(key: String, wmvs: WordMappingValueSet, wmv: WordMappingValue,
      userAnswer: UserAnswer): WordMappingGroup = {
    val wmvUpdated = wmv.addUserAnswer(userAnswer)
    updateWordMappingValue(key, wmvs, wmvUpdated)
  }

  def updateWordMappingValue(key: String, wmvsOld: WordMappingValueSet, wmv: WordMappingValue):
      WordMappingGroup = {
    val wmvsNew = WordMappingValueSetWrapper(wmvsOld.replaceWmv(wmv))
    addWordMappingToFront(WordMappingPair(key, wmvsNew))
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

  def addWordMapping(key: String, wordMappingValueSet: Option[WordMappingValueSetWrapperBase]):
      WordMappingGroup =
    wordMappingValueSet.map(addWordMappingToEnd(wordMappings, key, _)).getOrElse(this)

  protected def addWordMappingToEnd(key: String, value: String): WordMappingGroup = {
    val wmvsNew = updatedWordMappingValueSet(key, value)
    addWordMappingToEnd(key, wmvsNew)
  }

  protected def addWordMappingToEnd(key: String,
      wordMappingValueSet: WordMappingValueSetWrapperBase): WordMappingGroup =
    addWordMappingToEnd(wordMappings, key, wordMappingValueSet)

  protected def addWordMappingToEnd(wordMappings: Stream[WordMappingPair],
      key: String, wmvsNew: WordMappingValueSetWrapperBase): WordMappingGroup =
    thisUpdated(wordMappings.filterNot(_.key == key) :+ WordMappingPair(key, wmvsNew))


  def addWordMappingToFront(key: String, value: String): WordMappingGroup = {
    val wmvsNew = updatedWordMappingValueSet(key, value)
    thisUpdated(WordMappingPair(key, wmvsNew) +: wordMappings.filterNot(_.key == key))
  }

  protected def addWordMappingToFront(wmp: WordMappingPair): WordMappingGroup =
    addWordMappingToFront(wordMappings, wmp)

  protected def addWordMappingToFront(wordMappings: Stream[WordMappingPair],
      wmp: WordMappingPair): WordMappingGroup =
    thisUpdated(wmp +: wordMappings.filterNot(_.key == wmp.key))

  def removeWordMapping(key: String) = thisUpdated(wordMappings.filter(_.key != key))

  def removeWordMappingValue(keyWord: String, wordMappingValue: WordMappingValue):
      (WordMappingGroup, Boolean) =
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
    wordMappings.find(_.key == keyWord).map(_.valueSet)

  def findPresentableQuizItem: Option[QuizItemViewWithOptions] = {
    log("currentSearchRange: " + currentSearchRange.start)
    val wmSlice = wordMappings.slice(currentSearchRange.start, currentSearchRange.end)
    val quizItem =
      (for {
        wordMappingPair <- wmSlice.toStream
        quizItem <- findPresentableQuizItem(wordMappingPair, currentPromptNumber)
      } yield quizItem).headOption
    log("found quiz item " + quizItem)
    quizItem
  }

  def rangeForNextSearch: Range =
    if (currentSearchRange.end > wordMappings.size) 0 until rangeSize
    else currentSearchRange.start + rangeSize until currentSearchRange.end + rangeSize

  private def findPresentableQuizItem(wmp: WordMappingPair, currentPromptNumber: Int):
      Option[QuizItemViewWithOptions] = {
    //log("findPresentableQuizItem: key=" + key + ", currentPromptNumber=" +
    //    currentPromptNumber)
    val wordMappingValue = wmp.valueSet.findPresentableWordMappingValue(currentPromptNumber)
    wordMappingValue.map { wordMappingValue =>
      Util.stopwatch(quizItemWithOptions(wmp, wordMappingValue),
          "quizItemWithOptions for " + wordMappingValue)
    }
  }

  def findAnyUnfinishedQuizItem: Option[QuizItemViewWithOptions] = {
    log("findAnyUnfinishedQuizItem " + header)
    wordMappings.iterator.map(wmPair =>
      findAnyUnfinishedQuizItem(wmPair)).find(_.isDefined).getOrElse(None)
  }

  private def findAnyUnfinishedQuizItem(wmp: WordMappingPair): Option[QuizItemViewWithOptions] =
    wmp.valueSet.findAnyUnfinishedWordMappingValue.map(quizItemWithOptions(wmp, _))

  private def quizItemWithOptions(wmp: WordMappingPair,
      wordMappingValueCorrect: WordMappingValue): QuizItemViewWithOptions = {
    val numCorrectAnswers = wordMappingValueCorrect.numCorrectAnswersInARow
    val falseAnswers = makeFalseAnswers(wmp, wordMappingValueCorrect, numCorrectAnswers)
    new QuizItemViewWithOptions(wmp, wordMappingValueCorrect,
        currentPromptNumber, header, falseAnswers, numCorrectAnswers)
  }

  def makeFalseAnswers(wmpCorrect: WordMappingPair,
      wordMappingValueCorrect: WordMappingValue, numCorrectAnswersSoFar: Int):
    Set[String] = {

    var falseAnswers = new ListSet[String]
    val numFalseAnswersRequired = 2

    /*
     * If the user has already been having success with this word, first try to
     * fill the falseAnswers with similar-looking words.
     */
    if (numCorrectAnswersSoFar >= 1)
      falseAnswers ++= Util.stopwatch(makeFalseSimilarAnswers(wmpCorrect.valueSet,
          wordMappingValueCorrect, numCorrectAnswersSoFar, numFalseAnswersRequired),
          "makeFalseSimilarAnswers")

    // try again to fill the falseAnswers
    var totalTries = 20 // to stop any infinite loop
    while (falseAnswers.size < numFalseAnswersRequired && totalTries > 0) {
      totalTries = totalTries - 1
      val randomAnswer = findRandomWordValue(randomValues(100))
      randomAnswer.foreach( randomAnswer =>
        if (!wmpCorrect.valueSet.containsValue(randomAnswer))
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

    def hasSameStart = (wmv: WordMappingValue, value: String) => wmv.hasSameStart(value)
    def hasSameEnd = (wmv: WordMappingValue, value: String) => wmv.hasSameEnd(value)
    val similarityFunction = if (numCorrectAnswersSoFar % 2 == 1) hasSameStart else hasSameEnd

    var numValueSetsSearched = 0
    wordMappingValueSets.iterator.takeWhile(_ => similarWords.size < numFalseAnswersRequired).
      foreach(wmvs => {
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
      logError("randomValues found nothing")
    combinedValueSets
  }

  def randomSliceOfValueSets(sliceSize: Int): Iterable[WordMappingValueSetWrapperBase] =
    if (sliceSize >= size) wordMappingValueSets.toList
    else {
      val randomStart = Random.nextInt(size - sliceSize)
      val slice = wordMappings.slice(randomStart, randomStart + sliceSize)
      val valueSetsSlice = slice.map(_.valueSet)
      valueSetsSlice.toList
    }

  def merge(otherWmg: Option[WordMappingGroup]): WordMappingGroup =
    otherWmg.map(merge(_)).getOrElse(this)

  def merge(otherWmg: WordMappingGroup): WordMappingGroup = {
    val wordMappingsCombined = wordMappings ++ otherWmg.wordMappings
    thisUpdated(wordMappingsCombined)
  }

  def hasKey(key: String): Boolean = wordMappingKeys.find(_ == key).isDefined
  def keyBeginningWith(keyStart: String) = wordMappingKeys.find(_.startsWith(keyStart))
}


object WordMappingGroup extends Platform {

  val rangeSize = 200

  def splitterLineBreak = getSplitter('\n')
  def splitterKeyValue = getSplitter('|')

  /*
   * Example:
   *
   * wordMappingGroup keyType="English word" valueType="German word" currentPromptNumber="0"
   *    against|wider
   *    entertain|unterhalten
   */
  def fromCustomFormat(str: String): WordMappingGroup = {

    val splitterLineBreakLocal = getSplitter('\n')
    val splitterKeyValueLocal = getSplitter('|')

    // TODO: write directly to the Stream not a ListBuffer
    val wordMappingsMutable = new ListBuffer[WordMappingPair]()

    def parseWordMappingGroup {
      splitterLineBreakLocal.setString(str)
      splitterLineBreakLocal.next // skip the first line, which has already been parsed

      while (splitterLineBreakLocal.hasNext) {
        val strKeyValue = splitterLineBreakLocal.next
        splitterKeyValueLocal.setString(strKeyValue)

        if (splitterKeyValueLocal.hasNext) {

          def parseKeyValue {
            val strKey = splitterKeyValueLocal.next

            if (splitterKeyValueLocal.hasNext) {
              val strValues = splitterKeyValueLocal.next
              wordMappingsMutable += WordMappingPair(strKey, WordMappingValueSetLazyProxy(strValues))
            }
          }
          Try(parseKeyValue) recover {
            case e: Exception => logError("could not parse key-value string: " + strKeyValue)
          }
        }
      }
    }
    Try(parseWordMappingGroup) recover {
      case e: Exception => logError("could not parse wmg with str " + str.take(100) + "..." +
          str.takeRight(100))
    }

    val wordMappingsStream = wordMappingsMutable.toStream

    // Now use the persistent data structure.
    new WordMappingGroup(QuizGroupHeader(str), wordMappings = wordMappingsStream,
        currentPromptNumber = WordMappingGroup.parseCurrentPromptNumber(str))
  }

  def parseCurrentPromptNumber(str: String): Int =
    Try(StringUtil.parseValue(str, "currentPromptNumber=\"", "\"").toInt).recover {
      case e: Exception => logError("Could not parse prompt number from " + str)
                           0
    }.get
}