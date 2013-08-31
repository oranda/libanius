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

package com.oranda.libanius.model

import scala.collection.immutable._
import scala.util.{Try, Random}

import com.oranda.libanius.util.{StringUtil, Util}
import com.oranda.libanius.dependencies.AppDependencies
import com.oranda.libanius.model.wordmapping.{WordMappingGroup, Dictionary}

case class QuizGroup(
    header: QuizGroupHeader,
    quizPairs: Stream[QuizPair] = Stream.empty,
    currentPromptNumber: Int = 0,
    dictionary: Dictionary = new Dictionary)
  extends ModelComponent {

  private[this] lazy val l = AppDependencies.logger

  def keyType = header.keyType     // example: "English word"
  def valueType = header.valueType // example: "German word"

  def updatedQuizPairs(newQuizPairs: Stream[QuizPair]): QuizGroup =
    new QuizGroup(header, newQuizPairs, currentPromptNumber, dictionary)

  def updatedPromptNumber: QuizGroup =
    new QuizGroup(header, quizPairs, currentPromptNumber + 1, dictionary)


  def updatedWithUserAnswer(key: String, quizValue: QuizValueWithUserAnswers,
      userAnswer: UserAnswer) = {
    val quizValueUpdated = quizValue.addUserAnswer(userAnswer)
    addQuizPair(key, quizValueUpdated)
  }

  def addQuizPair(key: String, value: String): QuizGroup =
    addQuizPair(key, QuizValueWithUserAnswers(value))

  def addQuizPair(key: String, value: QuizValueWithUserAnswers): QuizGroup =
    if (!key.isEmpty && !value.value.isEmpty && key.toLowerCase != value.value.toLowerCase)
      addQuizPairToFront(QuizPair(key, value))
    else
      this

  protected[model] def addQuizPairToFront(key: String, value: String): QuizGroup =
    addQuizPairToFront(QuizPair(key, value))

  protected[model] def addQuizPairToFront(quizPair: QuizPair): QuizGroup =
    addQuizPairToFront(quizPairs, quizPair)

  protected[model] def addQuizPairToFront(quizPairs: Stream[QuizPair],
      quizPair: QuizPair): QuizGroup =
    updatedQuizPairs(quizPair +: quizPairs.filterNot(_.sameKeyValue(quizPair)))

  protected[model] def addQuizPairToEnd(quizPair: QuizPair): QuizGroup =
    addQuizPairToEnd(quizPairs, quizPair)

  protected[model] def addQuizPairToEnd(quizPairs: Stream[QuizPair], quizPair: QuizPair):
      QuizGroup = {
    val newQuizPairs = quizPairs.filterNot(_.sameKeyValue(quizPair)) :+ quizPair
    updatedQuizPairs(newQuizPairs)
  }


  def removeQuizPair(pair: QuizPair) = updatedQuizPairs(quizPairs.filterNot(_.sameKeyValue(pair)))
  def removeQuizPairsForKey(key: String) = updatedQuizPairs(quizPairs.filter(_.key != key))
  def removeQuizPairsForValue(value: String) =
    updatedQuizPairs(quizPairs.filter(_.value.value != value))

  def updatedDictionary(newDictionary: Dictionary) =
    new QuizGroup(header, quizPairs, currentPromptNumber, newDictionary)

  def quizKeys: Stream[String] = quizPairs.map(_.key)
  def quizValues: Stream[QuizValueWithUserAnswers] = quizPairs.map(_.value)

  def matches(qgOther: QuizGroup): Boolean = header.matches(qgOther.header)

  /*
   * Example of custom format:
   *
   * quizGroup type="WordMapping" keyType="English word" valueType="German word" currentPromptNumber="0"
   *    against|wider
   *    entertain|unterhalten
   */
  def toCustomFormat(strBuilder: StringBuilder) = {
    val wordMappingGroup = WordMappingGroup.fromQuizGroup(this)
    header.toCustomFormat(strBuilder).append(" currentPromptNumber=\"").
        append(currentPromptNumber).append("\"")
    // Imperative code is used for speed
    val iter = wordMappingGroup.wordMappingPairs.iterator
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

  def contains(key: String): Boolean = quizKeys.contains(key)
  def size = quizPairs.size
  def numKeyWords = size
  def numValues = size
  def numCorrectAnswers: Int = quizPairs.map(_.value.numCorrectAnswersInARow).sum


  // Low usage expected. Slow because we are not using a Map for quizPairs.
  def findValuesFor(keyWord: String): List[QuizValueWithUserAnswers] =
    quizPairs.filter(_.key == keyWord).map(_.value).toList

  def findAnyUnfinishedQuizItem: Option[QuizItemViewWithChoices] = {
    l.log("findAnyUnfinishedQuizItem " + header)
    quizPairs.iterator.find(pair => pair.value.isUnfinished).map(
        pair => quizItemWithOptions(pair, pair.value))
  }

  protected def quizItemWithOptions(wmp: QuizPair,
      quizValueCorrect: QuizValueWithUserAnswers): QuizItemViewWithChoices = {
    val numCorrectAnswers = quizValueCorrect.numCorrectAnswersInARow
    val falseAnswers = makeFalseAnswers(wmp, quizValueCorrect, numCorrectAnswers)
    new QuizItemViewWithChoices(wmp, quizValueCorrect,
      currentPromptNumber, header, falseAnswers, numCorrectAnswers)
  }


  protected def findPresentableQuizItem(quizPair: QuizPair, currentPromptNumber: Int):
      Option[QuizItemViewWithChoices] = {
    if (quizPair.value.isPresentable(currentPromptNumber))
      Util.stopwatch(Some(quizItemWithOptions(quizPair, quizPair.value)),
          "quizItemWithOptions for " + quizPair.value)
    else
      None
  }

  def makeFalseAnswers(wmpCorrect: QuizPair, quizValueCorrect: QuizValueWithUserAnswers,
      numCorrectAnswersSoFar: Int): Set[String] = {

    var falseAnswers = new ListSet[String]
    val numFalseAnswersRequired = 2

    /*
     * If the user has already been having success with this word, first try to
     * fill the falseAnswers with similar-looking words.
     */
    if (numCorrectAnswersSoFar >= 1) {
      val correctValues = findValuesFor(wmpCorrect.key)
      falseAnswers ++= Util.stopwatch(makeFalseSimilarAnswers(correctValues,
          quizValueCorrect, numCorrectAnswersSoFar, numFalseAnswersRequired),
          "makeFalseSimilarAnswers")
    }

    // try again to fill the falseAnswers
    var totalTries = 20 // to stop any infinite loop
    while (falseAnswers.size < numFalseAnswersRequired && totalTries > 0) {
      totalTries = totalTries - 1
      val randomAnswer: Option[String] = findRandomWordValue(randomValues(100))
      randomAnswer.foreach( randomAnswer =>
        if (wmpCorrect.value.value != randomAnswer)
          falseAnswers += randomAnswer
      )
    }

    // final try to fill false answers: use dummy data
    if (falseAnswers.isEmpty) falseAnswers += ""
    while (falseAnswers.size < numFalseAnswersRequired)
      falseAnswers += (falseAnswers.last + " ")

    falseAnswers
  }


  def makeFalseSimilarAnswers(correctQuizValues: List[QuizValueWithUserAnswers],
      correctValue: QuizValueWithUserAnswers,
      numCorrectAnswersSoFar: Int, numFalseAnswersRequired: Int): Set[String] = {

    var similarWords = new HashSet[String]

    def hasSameStart = (wmv: QuizValueWithUserAnswers, value: String) => wmv.hasSameStart(value)
    def hasSameEnd = (wmv: QuizValueWithUserAnswers, value: String) => wmv.hasSameEnd(value)
    val similarityFunction = if (numCorrectAnswersSoFar % 2 == 1) hasSameStart else hasSameEnd

    var numValueSetsSearched = 0
    val numSimilarLettersRequired = 2
    quizPairs.iterator.takeWhile(_ => similarWords.size < numFalseAnswersRequired).
        foreach(quizPair => {
      numValueSetsSearched = numValueSetsSearched + 1
      // Avoid selecting values belonging to the "correct" value set
      val correctValues = correctQuizValues.map(_.value)
      if (!correctValues.contains(quizPair.value)) {
        if (similarWords.size < numFalseAnswersRequired &&
            similarityFunction(quizPair.value, correctValue.value)(numSimilarLettersRequired))
          similarWords += quizPair.value.value
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

  def merge(otherWmg: Option[QuizGroup]): QuizGroup =
    otherWmg.map(merge(_)).getOrElse(this)

  def merge(otherWmg: QuizGroup): QuizGroup = {
    val quizPairsCombined = (quizPairs ++ otherWmg.quizPairs).asInstanceOf[Stream[QuizPair]]
    updatedQuizPairs(quizPairsCombined)
  }

  def hasKey(key: String): Boolean = quizKeys.find(_ == key).isDefined
  def keyBeginningWith(keyStart: String) = quizKeys.find(_.startsWith(keyStart))


  def findPresentableQuizItem: Option[QuizItemViewWithChoices] = {
    val quizItem =
      (for {
        quizPair <- quizPairs.toStream
        quizItem <- findPresentableQuizItem(quizPair, currentPromptNumber)
      } yield quizItem).headOption
    l.log("found quiz item " + quizItem)
    quizItem
  }


  def randomValues(sliceSize: Int): List[QuizValueWithUserAnswers] =
    randomSliceOfQuizPairs(sliceSize).map(_.value).toList

  def randomSliceOfQuizPairs(sliceSize: Int): Iterable[QuizPair] =
    if (sliceSize >= size) quizPairs.toList
    else {
      val randomStart = Random.nextInt(size - sliceSize)
      quizPairs.slice(randomStart, randomStart + sliceSize)
    }

}


object QuizGroup {

  private[this] val l = AppDependencies.logger

  def parseCurrentPromptNumber(str: String): Int =
    Try(StringUtil.parseValue(str, "currentPromptNumber=\"", "\"").toInt).recover {
      case e: Exception => l.logError("Could not parse prompt number from " + str)
                           0
    }.get

  /*
  def apply(header: QuizGroupHeader): QuizGroup = {
    // TODO: fix this
    header.quizGroupType match {
      case WordMapping => QuizGroup(header)
      case QuestionAndAnswer => QuizGroup(header)
    }
  } */

  // TODO: improve... is it really necessary to go through WordMappingGroup?
  def fromCustomFormat(text: String): QuizGroup =
    WordMappingGroup.fromCustomFormat(text).toQuizGroup

}