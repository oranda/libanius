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
import scala.util.{Try, Random}

import com.oranda.libanius.util.{StringUtil, Util}
import com.oranda.libanius.dependencies.AppDependencies
import com.oranda.libanius.model._
import scala.collection.immutable.Range
import scala.collection.immutable.Stream
import scala.collection.immutable.Seq
import scala.Some
import com.oranda.libanius.model.QuizPair
import com.oranda.libanius.model.QuizItemViewWithChoices

abstract class QuizGroup[S <: QuizValueSet](
    val header: QuizGroupHeader,
    val quizPairs: Stream[QuizPair[S]] = Stream.empty,
    val currentPromptNumber: Int = 0)
  extends ModelComponent {

  type MyType <: QuizGroup[S]

  private[this] lazy val l = AppDependencies.logger

  def keyType = header.keyType     // example: "English word"
  def valueType = header.valueType // example: "German word"

  // workaround: compiler does not accept import
  lazy val rangeSize = WordMappingGroup.rangeSize

  def updatedPromptNumber: MyType

  def quizKeys: Stream[String] = quizPairs.map(_.key)
  def quizValueSets: Stream[S] = quizPairs.map(_.valueSet)

  def matches(qgOther: QuizGroup[S]): Boolean = header.matches(qgOther.header)

  /*
   * Example of custom format:
   *
   * quizGroup type="WordMapping" keyType="English word" valueType="German word" currentPromptNumber="0"
   *    against|wider
   *    entertain|unterhalten
   */
  def toCustomFormat(strBuilder: StringBuilder) = {
    header.toCustomFormat(strBuilder).append(" currentPromptNumber=\"").
        append(currentPromptNumber).append("\"")
    val iter = quizPairs.iterator
    while (iter.hasNext) {
      val quizPair = iter.next
      strBuilder.append('\n').append(quizPair.key).append('|')
      quizPair.valueSet.toCustomFormat(strBuilder)
    }
    strBuilder
  }

  def getSaveData: SaveData = {
    val serialized = toCustomFormat(new StringBuilder())
    val fileName = header.makeQgFileName
    SaveData(fileName, serialized.toString)
  }

  def size = quizPairs.size
  def numKeyWords = quizPairs.size

  def numValues = quizValueSets.iterator.foldLeft(0)(_ + _.size)

  def numItemsAndCorrectAnswers: Pair[Int, Int] = {
    /*
     * The functional version is about 50% slower than the imperative version
     *
     * quizPairs.values.iterator.foldLeft(Pair(0, 0))((acc, value) =>
     *    (acc._1 + value.numItemsAndCorrectAnswers._1,
     *     acc._2 + value.numItemsAndCorrectAnswers._2))
     */
    var numItems = 0
    var numCorrectAnswers = 0
    val quizPairsIter = quizPairs.iterator
    while (quizPairsIter.hasNext) {
      val wmvs = quizPairsIter.next.valueSet
      val _numItemsAndCorrectAnswers = wmvs.numItemsAndCorrectAnswers
      numItems += _numItemsAndCorrectAnswers._1
      numCorrectAnswers +=_numItemsAndCorrectAnswers._2
    }
    Pair(numItems, numCorrectAnswers)
  }

  def contains(key: String): Boolean = quizKeys.contains(key)

  protected def addWordMappingToEnd(key: String, value: String): MyType

  // Low usage expected. Slow because we are not using a Map for quizPairs.
  def findValueSetFor(keyWord: String): Option[S] =
    quizPairs.find(_.key == keyWord).map(_.valueSet)

  def findAnyUnfinishedQuizItem: Option[QuizItemViewWithChoices[QuizPair[S]]] = {
    l.log("findAnyUnfinishedQuizItem " + header)
    quizPairs.iterator.map(wmPair =>
      findAnyUnfinishedQuizItem(wmPair)).find(_.isDefined).getOrElse(None)
  }

  private def findAnyUnfinishedQuizItem(wmp: QuizPair[S]):
      Option[QuizItemViewWithChoices[QuizPair[S]]] =
    wmp.valueSet.findAnyUnfinishedWordMappingValue.map(quizItemWithOptions(wmp, _))

  protected def quizItemWithOptions(wmp: QuizPair[S],
      quizValueCorrect: QuizValueWithUserAnswers): QuizItemViewWithChoices[QuizPair[S]] = {
    val numCorrectAnswers = quizValueCorrect.numCorrectAnswersInARow
    val falseAnswers = makeFalseAnswers(wmp, quizValueCorrect, numCorrectAnswers)
    new QuizItemViewWithChoices[QuizPair[S]](wmp, quizValueCorrect,
      currentPromptNumber, header, falseAnswers, numCorrectAnswers)
  }

  def findPresentableQuizItem: Option[QuizItemViewWithChoices[QuizPair[S]]] = {
    val quizItem =
      (for {
        quizPair <- quizPairs.toStream
        quizItem <- findPresentableQuizItem(quizPair, currentPromptNumber)
      } yield quizItem).headOption
    l.log("found quiz item " + quizItem)
    quizItem
  }

  protected def findPresentableQuizItem(quizPair: QuizPair[S], currentPromptNumber: Int):
      Option[QuizItemViewWithChoices[QuizPair[S]]] = {
    //log("findPresentableQuizItem: key=" + key + ", currentPromptNumber=" +
    //    currentPromptNumber)
    val quizValue = quizPair.valueSet.findPresentableWordMappingValue(currentPromptNumber)
    quizValue.map { quizValue =>
      Util.stopwatch(quizItemWithOptions(quizPair, quizValue), "quizItemWithOptions for " + quizValue)
    }
  }

  def makeFalseAnswers(wmpCorrect: QuizPair[S], quizValueCorrect: QuizValueWithUserAnswers,
      numCorrectAnswersSoFar: Int): Set[String] = {

    var falseAnswers = new ListSet[String]
    val numFalseAnswersRequired = 2

    /*
     * If the user has already been having success with this word, first try to
     * fill the falseAnswers with similar-looking words.
     */
    if (numCorrectAnswersSoFar >= 1)
      falseAnswers ++= Util.stopwatch(makeFalseSimilarAnswers(wmpCorrect.valueSet,
          quizValueCorrect, numCorrectAnswersSoFar, numFalseAnswersRequired),
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

  protected def randomValues(sliceSize: Int): Seq[QuizValueWithUserAnswers]

  def makeFalseSimilarAnswers(correctValues: S,
      correctValue: QuizValueWithUserAnswers, numCorrectAnswersSoFar: Int,
      numFalseAnswersRequired: Int): Set[String] = {
    var similarWords = new HashSet[String]

    def hasSameStart = (wmv: QuizValueWithUserAnswers, value: String) => wmv.hasSameStart(value)
    def hasSameEnd = (wmv: QuizValueWithUserAnswers, value: String) => wmv.hasSameEnd(value)
    val similarityFunction = if (numCorrectAnswersSoFar % 2 == 1) hasSameStart else hasSameEnd

    var numValueSetsSearched = 0
    quizValueSets.iterator.takeWhile(_ => similarWords.size < numFalseAnswersRequired).
        foreach(wmvs => {
      numValueSetsSearched = numValueSetsSearched + 1
      // Avoid selecting values belonging to the "correct" value set
      if (wmvs ne correctValues) {
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

  def findRandomWordValue(quizValues: Seq[QuizValueWithUserAnswers]): Option[String] = {
    if (quizValues.isEmpty)
      None
    else {
      val randomIndex = Random.nextInt(quizValues.length)
      Some(quizValues(randomIndex).value)
    }
  }

  def hasKey(key: String): Boolean = quizKeys.find(_ == key).isDefined
  def keyBeginningWith(keyStart: String) = quizKeys.find(_.startsWith(keyStart))
}


object QuizGroup {

  private[this] val l = AppDependencies.logger

  def parseCurrentPromptNumber(str: String): Int =
    Try(StringUtil.parseValue(str, "currentPromptNumber=\"", "\"").toInt).recover {
      case e: Exception => l.logError("Could not parse prompt number from " + str)
        0
    }.get
}