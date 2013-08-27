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

import com.oranda.libanius.model._
import scala.collection.immutable.{Stream, Range, Iterable, List}
import com.oranda.libanius.dependencies.AppDependencies
import scala.collection.mutable.ListBuffer
import scala.util.{Random, Try}
import com.oranda.libanius.util.Util

/*
 * A type of QuizGroup where the quiz pairs are mappings from a word to possible translations.
 */
case class WordMappingGroup(
    override val header: QuizGroupHeader,
    override val quizPairs: Stream[WordMappingQuizPair] = Stream.empty,
    override val currentPromptNumber: Int = 0,
    val dictionary: Dictionary = new Dictionary,
    val currentSearchRange: Range = WordMappingGroup.initRange)
  extends QuizGroup[WordMappingValueSet](header, quizPairs, currentPromptNumber) {

  private[this] lazy val l = AppDependencies.logger

  override type MyType = WordMappingGroup

  type S = WordMappingValueSet

  protected def updatedQuizPairs(newQuizPairs: Stream[WordMappingQuizPair]): MyType =
    new WordMappingGroup(header, newQuizPairs,
        currentPromptNumber, dictionary, currentSearchRange)

  override def updatedPromptNumber: MyType =
    new WordMappingGroup(header, quizPairs, currentPromptNumber + 1, dictionary,
        currentSearchRange)

  def resetSearchRange =
    new WordMappingGroup(header, quizPairs, currentPromptNumber, dictionary,
      WordMappingGroup.initRange)

  def updatedDictionary(newDictionary: Dictionary) =
    new WordMappingGroup(header, quizPairs, currentPromptNumber, newDictionary,
      WordMappingGroup.initRange)

  def updatedSearchRange =
    new WordMappingGroup(header, quizPairs, currentPromptNumber, dictionary, rangeForNextSearch)

  private def updateWordMappingValue(key: String, wmvsOld: S, wmv: QuizValueWithUserAnswers) = {
    val wmvsNew = WordMappingValueSetWrapper(wmvsOld.replaceWmv(wmv))
    val quizPair = new WordMappingQuizPair(key, wmvsNew)
    addWordMappingToFront(quizPair)
  }

  def addWordMapping(key: String, value: String): MyType =
    if (!key.isEmpty && !value.isEmpty && key.toLowerCase != value.toLowerCase)
      addWordMappingToEnd(key, value)
    else
      this

  protected def addWordMappingToFront(wmp: WordMappingQuizPair): WordMappingGroup =
    addWordMappingToFront(quizPairs, wmp)


  protected def addWordMappingToFront(wordMappings: Stream[WordMappingQuizPair],
                                      wmp: WordMappingQuizPair) =
    updatedQuizPairs(wmp +: wordMappings.filterNot(_.key == wmp.key))

  def updateWithUserAnswer(key: String, wmvs: S, wmv: QuizValueWithUserAnswers,
                           userAnswer: UserAnswer) = {
    val wmvUpdated = wmv.addUserAnswer(userAnswer)
    updateWordMappingValue(key, wmvs, wmvUpdated)
  }

  def removeWordMapping(key: String) = updatedQuizPairs(quizPairs.filter(_.key != key))

  def removeWordMappingValue(keyWord: String, wordMappingValue: QuizValueWithUserAnswers):
  (WordMappingGroup, Boolean) =
    (findValueSetFor(keyWord) map {
      case wm => val wmvsNew = removeValue(wm, wordMappingValue)
        (addWordMappingValueSetToEnd(keyWord, wmvsNew), true)
    }).getOrElse((this, false))


  def addWordMapping(key: String, wordMappingValueSet: Option[S]): MyType =
    wordMappingValueSet.map(addWordMappingToEnd(quizPairs, key, _)).getOrElse(this)

  def addWordMappingToFront(key: String, value: String): WordMappingGroup = {
    val wmvsNew = updatedWordMappingValueSet(key, value)
    updatedQuizPairs(WordMappingQuizPair(key, wmvsNew) +: quizPairs.filterNot(_.key == key))
  }

  protected def addWordMappingToEnd(wordMappings: Stream[WordMappingQuizPair], key: String,
                                    wmvsNew: S): WordMappingGroup =
    updatedQuizPairs(wordMappings.filterNot(_.key == key) :+ WordMappingQuizPair(key, wmvsNew))

  def rangeForNextSearch: Range =
    if (currentSearchRange.end > quizPairs.size) 0 until rangeSize
    else currentSearchRange.start + rangeSize until currentSearchRange.end + rangeSize

  protected def addWordMappingToEnd(key: String, value: String) = {
    val wmvsNew = updatedWordMappingValueSet(key, value)
    addWordMappingValueSetToEnd(key, wmvsNew)
  }

  protected def addWordMappingValueSetToEnd(key: String, wordMappingValueSet: S):
      WordMappingGroup =
    addWordMappingToEnd(quizPairs, key, wordMappingValueSet)


  protected def updatedWordMappingValueSet(key: String, value: String): S =
    updatedWordMappingValueSet(key, QuizValueWithUserAnswers(value))

  def updatedWordMappingValueSet(key: String, wmvNew: QuizValueWithUserAnswers): S = {
    val wmvsOld = findValueSetFor(key)
    WordMappingValueSetWrapper(
      wmvsOld match {
        case Some(wmvsOld) => wmvsOld.addValueToFront(wmvNew)
        case _ => WordMappingValueSet(List(wmvNew))
      })
  }

  def removeValue(wmvs: S, wmv: QuizValueWithUserAnswers): WordMappingValueSetWrapper =
    WordMappingValueSetWrapper(wmvs.removeValue(wmv))

  override def randomValues(sliceSize: Int): List[QuizValueWithUserAnswers] = {
    val combinedValueSets = WordMappingValueSet.combineValueSets(randomSliceOfValueSets(sliceSize))
    if (combinedValueSets.isEmpty)
      l.logError("randomValues found nothing")
    combinedValueSets
  }

  def randomSliceOfValueSets(sliceSize: Int): Iterable[WordMappingValueSet] =
    if (sliceSize >= size) quizValueSets.toList
    else {
      val randomStart = Random.nextInt(size - sliceSize)
      val slice = quizPairs.slice(randomStart, randomStart + sliceSize)
      val valueSetsSlice = slice.map(_.valueSet)
      valueSetsSlice.toList
    }

  def merge(otherWmg: Option[WordMappingGroup]): WordMappingGroup =
    otherWmg.map(merge(_)).getOrElse(this)

  def merge(otherWmg: WordMappingGroup): WordMappingGroup = {
    val wordMappingsCombined = quizPairs ++ otherWmg.quizPairs
    updatedQuizPairs(wordMappingsCombined)
  }

  override def findPresentableQuizItem: Option[QuizItemViewWithChoices[QuizPair[S]]] = {
    l.log("currentSearchRange: " + currentSearchRange.start)
    val wmSlice = quizPairs.slice(currentSearchRange.start, currentSearchRange.end)
    val quizItem =
      (for {
        quizPair <- wmSlice.toStream
        quizItem <- findPresentableQuizItem(quizPair, currentPromptNumber)
      } yield quizItem).headOption
    l.log("found quiz item " + quizItem)
    quizItem
  }
}

object WordMappingGroup {

  type S = WordMappingValueSetLazyProxy with WordMappingValueSet

  private[this] val l = AppDependencies.logger

  val rangeSize = 200
  val initRange = 0 until rangeSize

  def splitterLineBreak = AppDependencies.stringSplitterFactory.getSplitter('\n')
  def splitterKeyValue = AppDependencies.stringSplitterFactory.getSplitter('|')

  /*
   * Example:
   *
   * quizGroup type="WordMapping" keyType="English word" valueType="German word" currentPromptNumber="0"
   *    against|wider
   *    entertain|unterhalten
   */
  def fromCustomFormat(str: String): WordMappingGroup = {

    val splitterLineBreak = WordMappingGroup.splitterLineBreak
    val splitterKeyValue = WordMappingGroup.splitterKeyValue

    // TODO: write directly to the Stream not a ListBuffer
    val wordMappingsMutable = new ListBuffer[WordMappingQuizPair]()

    def parseQuizGroup {
      splitterLineBreak.setString(str)
      splitterLineBreak.next // skip the first line, which has already been parsed

      while (splitterLineBreak.hasNext) {
        val strKeyValue = splitterLineBreak.next
        splitterKeyValue.setString(strKeyValue)

        if (splitterKeyValue.hasNext) {

          def parseKeyValue {
            val strKey = splitterKeyValue.next

            if (splitterKeyValue.hasNext) {
              val strValues = splitterKeyValue.next
              wordMappingsMutable += WordMappingQuizPair(strKey,
                  WordMappingValueSetLazyProxy(strValues))
            }
          }
          Try(parseKeyValue) recover {
            case e: Exception => l.logError("could not parse key-value string: " + strKeyValue)
          }
        }
      }
    }
    Try(parseQuizGroup) recover {
      case e: Exception => l.logError("could not parse wmg with str " + str.take(100) + "..." +
        str.takeRight(100))
    }

    val wordMappingsStream = wordMappingsMutable.toStream

    // Now use the persistent data structure.
    new WordMappingGroup(QuizGroupHeader(str), quizPairs = wordMappingsStream,
      currentPromptNumber = QuizGroup.parseCurrentPromptNumber(str))
  }


}