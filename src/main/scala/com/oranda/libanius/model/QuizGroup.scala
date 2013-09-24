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
import scala.language.postfixOps
import scala.util.{Try, Random}

import java.lang.StringBuilder

import com.oranda.libanius.util.{StringUtil, Util}
import com.oranda.libanius.model.wordmapping.{WordMappingGroup, Dictionary}
import com.oranda.libanius.model.quizitem.{QuizItemViewWithChoices, Value, TextValue, QuizItem}
import com.oranda.libanius.dependencies.AppDependencyAccess

import scalaz._
import scalaz.std.set
import Scalaz._

/*
 * Contains quiz items for a topic in a given order. This is currently a Stream
 * of QuizItem's where each QuizItem includes a user's responses. (Formerly it was a ListMap
 * but this data structure was too slow for inserting large numbers of quiz items.)
 */
case class QuizGroup(quizItems: Stream[QuizItem] = Stream.empty,
    currentPromptNumber: Int = 0, dictionary: Dictionary = new Dictionary)
  extends ModelComponent {

  def updatedQuizItems(newQuizItems: Stream[QuizItem]): QuizGroup =
    QuizGroup.quizGroupItemsLens.set(this, newQuizItems)

  def updatedPromptNumber: QuizGroup = QuizGroup.promptNumberLens.mod((1+), this)

  def updatedWithUserAnswer(prompt: TextValue, response: TextValue, wasCorrect: Boolean,
      userResponses: UserResponses, userAnswer: UserResponse): QuizGroup = {
    val userResponseUpdated = userResponses.addUserAnswer(userAnswer, wasCorrect)
    addQuizItem(prompt, response, userResponseUpdated)
  }

  def updatedDictionary(newDictionary: Dictionary) = QuizGroup.dictionaryLens.set(this, newDictionary)

  def addQuizItem(prompt: TextValue, response: TextValue, userResponses: UserResponses = UserResponses()):
      QuizGroup =
    addQuizItemToFront(QuizItem(prompt, response, userResponses))

  def addNewQuizItem(prompt: String, response: String): QuizGroup =

    if (!prompt.isEmpty && !response.isEmpty && prompt.toLowerCase != response.toLowerCase)
      addQuizItemToFront(QuizItem(prompt, response))
    else
      this

  protected[model] def addQuizItemToFront(quizItem: QuizItem): QuizGroup =
    addQuizItemToFront(quizItems, quizItem)

  protected[model] def addQuizItemToFront(quizItems: Stream[QuizItem],
      quizItem: QuizItem): QuizGroup =
    QuizGroup.quizGroupItemsLens.set(this, quizItem +: remove(quizItem))

  protected[model] def addQuizItemToEnd(quizItem: QuizItem): QuizGroup =
    addQuizItemToEnd(quizItems, quizItem)

  protected[model] def addQuizItemToEnd(quizItems: Stream[QuizItem], quizItem: QuizItem):
      QuizGroup =
    updatedQuizItems(remove(quizItem) :+ quizItem)

  def removeQuizItem(quizItem: QuizItem) = QuizGroup.quizGroupItemsLens.set(this, remove(quizItem))

  def remove(quizItem: QuizItem): Stream[QuizItem] =
    quizItems.filterNot(_.samePromptAndResponse(quizItem))

  def removeQuizItemsForPrompt(prompt: String) =
    updatedQuizItems(quizItems.filter(_.prompt.value != prompt))
  def removeQuizItemsForResponse(response: String) =
    updatedQuizItems(quizItems.filter(_.response.value != response))

  def quizPrompts: Stream[TextValue] = quizItems.map(_.prompt)
  def quizResponses: Stream[TextValue] = quizItems.map(_.response)

  /*
   * Example of custom format:
   *
   * quizGroup type="WordMapping" promptType="English word" responseType="German word" currentPromptNumber="0"
   *    against|wider
   *    entertain|unterhalten
   */
  def toCustomFormat(strBuilder: StringBuilder, header: QuizGroupHeader) = {
    val wordMappingGroup = WordMappingGroup.fromQuizGroup(header, this)
    header.toCustomFormat(strBuilder).append(" currentPromptNumber=\"").
        append(currentPromptNumber).append("\"")
    // Imperative code is used for speed
    val iter = wordMappingGroup.wordMappingPairs.iterator
    while (iter.hasNext) {
      val wmPair = iter.next
      strBuilder.append('\n').append(wmPair.key).append('|')
      wmPair.valueSet.toCustomFormat(strBuilder)
    }
    strBuilder
  }

  def contains(quizItem: QuizItem): Boolean = quizItems.exists(_.samePromptAndResponse(quizItem))
  def contains(prompt: String): Boolean = contains(TextValue(prompt))
  def contains(prompt: TextValue): Boolean = quizPrompts.contains(prompt)
  def numQuizItems = quizItems.size
  def size = numQuizItems
  def isEmpty = quizItems.isEmpty
  def numPrompts = size
  def numResponses = size
  def numCorrectAnswers: Int = (0 /: quizItems)(_ + _.numCorrectAnswersInARow)

  /*
   * This may give similar results to findResponseFor but it uses the dictionary
   * and does not take into account any edits to the quiz group during a quiz run.
   */
  def findValuesFor(prompt: String): List[String] =
    dictionary.findValuesFor(prompt).values.map(_.value)

  /*
   * Low usage expected. Slow because we are not using a Map for quizItems.
   */
  def findResponsesFor(prompt: String): List[String] =
    quizItems.filter(_.prompt.matches(prompt)).map(_.response.value).toList

  /*
   * Low usage expected. Slow because we are not using a Map for quizItems.
   */
  def findPromptsFor(response: String): List[String] =
    quizItems.filter(_.response.matches(response)).map(_.prompt.value).toList

  def findAnyUnfinishedQuizItem(header: QuizGroupHeader): Option[QuizItemViewWithChoices] = {
    l.log("findAnyUnfinishedQuizItem for " + header)
    quizItems.iterator.find(_.userResponses.isUnfinished).map(
        pair => quizItemWithOptions(pair, header))
  }

  protected def quizItemWithOptions(quizItem: QuizItem, header: QuizGroupHeader):
       QuizItemViewWithChoices = {
    val numCorrectAnswers = quizItem.userResponses.numCorrectAnswersInARow
    val falseAnswers = makeFalseAnswers(quizItem, numCorrectAnswers)
    new QuizItemViewWithChoices(quizItem, currentPromptNumber, header, falseAnswers,
        numCorrectAnswers)
  }

  protected def findPresentableQuizItem(quizItem: QuizItem, header: QuizGroupHeader,
      currentPromptNumber: Int): Option[QuizItemViewWithChoices] =
    quizItem.isPresentable(currentPromptNumber) option
        Util.stopwatch(quizItemWithOptions(quizItem, header),
            "quizItemWithOptions for " + quizItem.response)

  protected[model] def makeFalseAnswers(itemCorrect: QuizItem,
      numCorrectAnswersSoFar: Int, numFalseAnswersRequired: Int = 2): Set[String] = {

    /*
     * If the user has already been having success with this word, first try to
     * find similar-looking words.
     */
    val falseSimilarAnswers: Set[String] =
      if (numCorrectAnswersSoFar == 0) ListSet[String]()
      else {
        val correctValues = findResponsesFor(itemCorrect.prompt.value)
        val correctValuePresented = itemCorrect.response.value
        Util.stopwatch(makeFalseSimilarAnswers(correctValues, correctValuePresented,
            numCorrectAnswersSoFar, numFalseAnswersRequired), "makeFalseSimilarAnswers")
      }

    val numFalseAnswersStillToBeFound1 = numFalseAnswersRequired - falseSimilarAnswers.size
    val randomFalseAnswers = makeRandomFalseAnswers(numFalseAnswersStillToBeFound1, itemCorrect)

    val falseAnswersSoFar: Set[String] = falseSimilarAnswers ++ randomFalseAnswers

    val numFalseAnswersStillToBeFound2 = numFalseAnswersRequired - falseAnswersSoFar.size

    // numFalseAnswersStillToBeFound2 should be 0, but deal with the edge case of bad data.
    val dummyAnswers = uniqueDummyAnswers(numFalseAnswersStillToBeFound2, falseAnswersSoFar)

    falseAnswersSoFar ++ dummyAnswers
  }

  protected[model] def makeRandomFalseAnswers(numFalseAnswersRequired: Int,
      itemCorrect: QuizItem): Set[String] = {

    def randomFalseWordValue(sliceIndex: Int) =
      findRandomWordValue(sliceOfResponses(sliceIndex, numSlices = numFalseAnswersRequired)).
          filter(itemCorrect.response.value != _)

    (0 until numFalseAnswersRequired).map(
        sliceIndex => randomFalseWordValue(sliceIndex)).flatten.toSet
  }

  protected[model] def uniqueDummyAnswers(numFalseAnswersRequired: Int,
      falseAnswersSoFar: Set[String]): Set[String] = {
    if (numFalseAnswersRequired == 0) Set.empty[String]
    else {
      val characters = "abcdefghijklmnopqrstuvwxyz0123456789".toCharArray
      if (numFalseAnswersRequired > characters.length) {
        l.logError("Too many dummy answers requested.")
        Set.empty[String]
      } else
        characters.map(_.toString).take(numFalseAnswersRequired).toSet
    }
  }

  protected[model] def makeFalseSimilarAnswers(correctValues: List[String], correctValue: String,
      numCorrectAnswersSoFar: Int, numFalseAnswersRequired: Int): Set[String] = {

    var similarWords = new HashSet[String]

    def hasSameStart = (value1: TextValue, value2: String) => value1.hasSameStart(value2)
    def hasSameEnd = (value1: TextValue, value2: String) => value1.hasSameEnd(value2)
    val similarityFunction = if (numCorrectAnswersSoFar % 2 == 1) hasSameStart else hasSameEnd

    var numValueSetsSearched = 0
    val numSimilarLettersRequired = 2
    quizItems.iterator.takeWhile(_ => similarWords.size < numFalseAnswersRequired).
        foreach(quizItem => {
      numValueSetsSearched = numValueSetsSearched + 1
      // Avoid selecting values belonging to the "correct" response set
      if (!correctValues.contains(quizItem.response)) {
        if (similarWords.size < numFalseAnswersRequired &&
            similarityFunction(quizItem.response, correctValue)(numSimilarLettersRequired))
          similarWords += quizItem.response.value
      }
    })
    similarWords
  }

  def findRandomWordValue(quizValues: Seq[TextValue]): Option[String] =
    !quizValues.isEmpty option quizValues(Random.nextInt(quizValues.length)).value

  def merge(otherWmg: Option[QuizGroup]): QuizGroup =
    otherWmg.map(merge(_)).getOrElse(this)

  def merge(otherWmg: QuizGroup): QuizGroup = {
    val quizItemsCombined = (quizItems ++ otherWmg.quizItems).asInstanceOf[Stream[QuizItem]]
    updatedQuizItems(quizItemsCombined)
  }

  def hasPrompt(prompt: String): Boolean = quizPrompts.contains(prompt)

  def findPresentableQuizItem(header: QuizGroupHeader): Option[QuizItemViewWithChoices] = {
    val quizItem =
      (for {
        quizItem <- quizItems.toStream
        quizItem <- findPresentableQuizItem(quizItem, header, currentPromptNumber)
      } yield quizItem).headOption
    l.log("found quiz item " + quizItem)
    quizItem
  }

  def sliceOfResponses(sliceIndex: Int, numSlices: Int): List[TextValue] =
    sliceOfQuizItems(sliceIndex, numSlices).map(_.response).toList

  def sliceOfQuizItems(sliceIndex: Int, numSlices: Int): Iterable[QuizItem] = {
    val sliceSize = size / numSlices
    val start = sliceIndex * sliceSize
    quizItems.slice(start, start + sliceSize)
  }

  def randomValues(sliceSize: Int): List[TextValue] =
    randomSliceOfQuizItems(sliceSize).map(_.response).toList

  def randomSliceOfQuizItems(sliceSize: Int): Iterable[QuizItem] =
    if (sliceSize >= size) quizItems.toList
    else {
      val randomStart = Random.nextInt(size - sliceSize)
      quizItems.slice(randomStart, randomStart + sliceSize)
    }
}

object QuizGroup extends AppDependencyAccess {

  val quizGroupItemsLens: Lens[QuizGroup, Stream[QuizItem]] = Lens.lensu(
      get = (_: QuizGroup).quizItems,
      set = (qGroup: QuizGroup, qItems: Stream[QuizItem]) => qGroup.copy(quizItems = qItems))

  val promptNumberLens: Lens[QuizGroup, Int] = Lens.lensu(
      get = (_: QuizGroup).currentPromptNumber,
      set = (qGroup: QuizGroup, promptNum: Int) => qGroup.copy(currentPromptNumber = promptNum))

  val dictionaryLens: Lens[QuizGroup, Dictionary] = Lens.lensu(
      get = (_: QuizGroup).dictionary,
      set = (qGroup: QuizGroup, d: Dictionary) => qGroup.copy(dictionary = d))

  def remove(quizItems: Stream[QuizItem], quizItem: QuizItem): Stream[QuizItem] =
    quizItems.filterNot(_.samePromptAndResponse(quizItem))

  def parseCurrentPromptNumber(str: String): Int =
    Try(StringUtil.parseValue(str, "currentPromptNumber=\"", "\"").toInt).recover {
      case e: Exception => l.logError("Could not parse prompt number from " + str)
                           0
    }.get


  def fromCustomFormat(text: String): QuizGroupWithHeader = {
    val wmg = WordMappingGroup.fromCustomFormat(text)
    QuizGroupWithHeader(wmg.header, wmg.toQuizGroup)
  }


}

// simple response object used for saving a data structure to a file
case class SaveData(fileName: String, data: String)