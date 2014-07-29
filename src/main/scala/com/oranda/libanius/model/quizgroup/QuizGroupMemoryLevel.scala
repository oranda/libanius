/*
 * Libanius
 * Copyright (C) 2012-2014 James McCabe <james@oranda.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.oranda.libanius.model.quizgroup

import scala.collection.immutable._
import scala.language.postfixOps
import scala.util.{Try, Random}

import com.oranda.libanius.model.quizitem.{QuizItem, TextValue}
import com.oranda.libanius.dependencies.AppDependencyAccess

import scalaz._
import scalaz.std.set
import scalaz.Scalaz._
import com.oranda.libanius.model._
import scala._
import scala.collection.immutable.Stream
import scala.collection.immutable.List
import scala.collection.immutable.Iterable
import java.lang.StringBuilder
import com.oranda.libanius.util.StringSplitter
import scalaz.PLens._
import scala.Some
import com.oranda.libanius.model.UserResponse

/*
 * QuizGroupMemoryLevel: a partition of a QuizGroup, containing quiz items which have
 * all been answered correctly a certain number of times.
 * (A ListMap was formerly used to store quiz items but this was found to be too
 * slow for bulk insert. Currently a Stream is used.)
 */
case class QuizGroupMemoryLevel(repetitionInterval: Int,
    quizItemStream: Stream[QuizItem] = Stream.empty, totalResponses: Int = 0,
    numCorrectResponses: Int = 0) extends ModelComponent {

  lazy val quizItems = quizItemStream.toList

  protected[quizgroup] def contains(quizItem: QuizItem): Boolean =
    quizItems.exists(_.samePromptAndResponse(quizItem))
  protected[quizgroup] def contains(prompt: TextValue): Boolean =
    quizItems.exists(_.prompt == prompt)
  protected[quizgroup] def contains(prompt: String): Boolean = contains(TextValue(prompt))
  protected[model] def numQuizItems = quizItems.size
  protected[quizgroup] def size = numQuizItems
  protected[quizgroup] def isEmpty = quizItems.isEmpty
  protected[quizgroup] def numPrompts = size
  protected[quizgroup] def numResponses = size

  protected[quizgroup] def updatedQuizItems(newQuizItems: List[QuizItem]): QuizGroupMemoryLevel =
    QuizGroupMemoryLevel.itemsLens.set(this, newQuizItems)

  protected[quizgroup] def updatedWithUserAnswer(prompt: TextValue, response: TextValue,
      wasCorrect: Boolean, userResponses: UserResponses, userAnswer: UserResponse):
      QuizGroupMemoryLevel = {
    val userResponseUpdated = userResponses.add(userAnswer, wasCorrect)
    addQuizItem(prompt, response, userResponseUpdated)
  }

  protected[quizgroup] def addQuizItem(prompt: TextValue, response: TextValue,
      userResponses: UserResponses = UserResponses()): QuizGroupMemoryLevel =
    addQuizItemToFront(QuizItem(prompt, response, userResponses))

  protected[quizgroup] def addNewQuizItem(prompt: String, response: String): QuizGroupMemoryLevel =
    if (!prompt.isEmpty && !response.isEmpty && prompt.toLowerCase != response.toLowerCase)
      addQuizItemToFront(QuizItem(prompt, response))
    else
      this

  protected[quizgroup] def addQuizItemToFront(quizItem: QuizItem): QuizGroupMemoryLevel =
    addQuizItemToFront(quizItems, quizItem)

  protected[quizgroup] def addQuizItemToFront(quizItems: List[QuizItem],
      quizItem: QuizItem): QuizGroupMemoryLevel =
    QuizGroupMemoryLevel.itemsLens.set(this, quizItem +: remove(quizItem))

  protected[quizgroup] def addQuizItemToEnd(quizItem: QuizItem): QuizGroupMemoryLevel =
    addQuizItemToEnd(quizItems, quizItem)

  protected[quizgroup] def addQuizItemToEnd(quizItems: List[QuizItem], quizItem: QuizItem):
      QuizGroupMemoryLevel =
    updatedQuizItems(remove(quizItem) :+ quizItem)

  protected[quizgroup] def removeQuizItem(quizItem: QuizItem): QuizGroupMemoryLevel =
    QuizGroupMemoryLevel.itemsLens.set(this, remove(quizItem))

  protected[quizgroup] def remove(quizItem: QuizItem): List[QuizItem] =
    quizItems.filterNot(_.samePromptAndResponse(quizItem))

  protected[quizgroup] def removeQuizItemsForPrompt(prompt: String) =
    updatedQuizItems(quizItems.filter(_.prompt.value != prompt))
  protected[quizgroup] def removeQuizItemsForResponse(response: String) =
    updatedQuizItems(quizItems.filter(_.correctResponse.value != response))

  protected[quizgroup] def quizPrompts: List[TextValue] = quizItems.map(_.prompt)
  protected[quizgroup] def quizResponses: List[TextValue] = quizItems.map(_.correctResponse)

  /*
   * Low usage expected. Slow because we are not using a Map for quizItems.
   */
  protected[quizgroup] def findResponsesFor(prompt: String): List[String] =
    quizItems.filter(_.prompt.matches(prompt)).map(_.correctResponse.value).toList

  /*
   * Low usage expected. Slow because we are not using a Map for quizItems.
   */
  protected[quizgroup] def findPromptsFor(response: String): List[String] =
    quizItems.filter(_.correctResponse.matches(response)).map(_.prompt.value).toList

  /*
   * Low usage expected. Slow because we are not using a Map for quizItems.
   */
  protected[quizgroup] def findQuizItem(prompt: String, response: String): Option[QuizItem] =
    quizItems.find(_.samePromptAndResponse(QuizItem(prompt, response)))

  protected[quizgroup] def findAnyUnfinishedQuizItem: Option[QuizItem] =
    quizItems.headOption

  def findPresentableQuizItem(currentPromptNumber: Int): Option[QuizItem] =
    (for {
      quizItem <- quizItems.toStream
      if quizItem.isPresentable(currentPromptNumber, repetitionInterval)
    } yield quizItem).headOption

  protected[quizgroup] def constructWrongChoicesSimilar(correctResponses: List[String],
      numWrongResponsesRequired: Int, correctValue: String,
      similarityPredicate: (TextValue, String) => Int => Boolean): List[String] = {

    var similarWords = new HashSet[String]
    var numValueSetsSearched = 0
    val numSimilarLettersRequired = 2
    quizItems.iterator.takeWhile(_ => similarWords.size < numWrongResponsesRequired).
      foreach(quizItem => {
        numValueSetsSearched = numValueSetsSearched + 1
        // Avoid selecting values belonging to the "correct" correctResponse set
        if (!correctResponses.contains(quizItem.correctResponse)) {
          if (similarWords.size < numWrongResponsesRequired &&
            similarityPredicate(quizItem.correctResponse, correctValue)(numSimilarLettersRequired))
            similarWords += quizItem.correctResponse.value
        }
      })
    similarWords.toList
  }

  protected[quizgroup] def randomValues(sliceSize: Int): List[TextValue] =
    randomSliceOfQuizItems(sliceSize).map(_.correctResponse).toList

  protected[quizgroup] def constructWrongChoicesRandom(correctValues: List[String],
      numFalseAnswersRequired: Int, itemCorrect: QuizItem): List[String] = {

    def randomFalseWordValue(sliceIndex: Int): Option[String] = {
      val sliceSize = (numQuizItems / numFalseAnswersRequired)
      val sliceStartIndex = sliceIndex * sliceSize
      val randomOffset = Random.nextInt(math.max(sliceSize, 1))
      val randomIndex = math.min(sliceStartIndex + randomOffset, numQuizItems - 1)
      val randomItem = Some(quizItems(randomIndex).correctResponse.value)
      randomItem.filter(!correctValues.contains(_))
    }

    (0 until numFalseAnswersRequired).map(
        sliceIndex => randomFalseWordValue(sliceIndex)).flatten.toList
  }

  protected[quizgroup] def randomSliceOfQuizItems(sliceSize: Int): Iterable[QuizItem] =
    if (sliceSize >= size) quizItems.toList
    else {
      val randomStart = Random.nextInt(size - sliceSize)
      quizItems.slice(randomStart, randomStart + sliceSize)
    }


  override def toString =
    repetitionInterval + ":" + numCorrectResponses + "/" + totalResponses + ": " +
    quizItems.map(_.prompt)

  def isAtLimit = totalResponses >= QuizGroupMemoryLevel.totalResponsesLimit

  def inc(isCorrect: Boolean) = {
    val numCorrectToAdd = if (isCorrect) 1 else 0
    // For each memory level, only check the recent performance. Reset the counters after a limit.
    if (!isAtLimit)
      QuizGroupMemoryLevel(repetitionInterval, quizItemStream, totalResponses + 1,
          numCorrectResponses + numCorrectToAdd)
    else // reset the counters
      QuizGroupMemoryLevel(repetitionInterval, quizItemStream, 1, numCorrectToAdd)
  }

  def wasNotTooRecentlyUsed(currentPromptNum : Int, promptNumInMostRecentResponse: Option[Int]) =
    promptNumInMostRecentResponse.forall {
      case promptNumInMostRecentResponse =>
        val diffInPromptNum = currentPromptNum - promptNumInMostRecentResponse
        diffInPromptNum >= repetitionInterval
    }

  def updatedInterval: Int = {
    def modifyBy(anInt: Int, aReal: Double) =
      if (totalResponses <= 5)
        repetitionInterval + anInt
      else
        (repetitionInterval * (1 + aReal)).toInt

    math.max(0,
      if (numCorrectResponses < 7)
        modifyBy(-1, -0.2)
      else if (numCorrectResponses > 8)
        modifyBy(1, 0.2)
      else
        repetitionInterval
    )
  }
}

object QuizGroupMemoryLevel extends AppDependencyAccess {

  val itemsLens: Lens[QuizGroupMemoryLevel, List[QuizItem]] = Lens.lensu(
    get = (_: QuizGroupMemoryLevel).quizItems,
    set = (qgp: QuizGroupMemoryLevel,
           qItems: List[QuizItem]) => qgp.copy(quizItemStream = qItems.toStream))

  protected[quizgroup] def remove(quizItems: List[QuizItem], quizItem: QuizItem):
      List[QuizItem] =
    quizItems.filterNot(_.samePromptAndResponse(quizItem))

  val totalResponsesLimit = 10

  val intervalLens: Lens[QuizGroupMemoryLevel, Int] = Lens.lensu(
    get = (_: QuizGroupMemoryLevel).repetitionInterval,
    set = (qgml: QuizGroupMemoryLevel, ri: Int) => qgml.copy(repetitionInterval = ri))

}