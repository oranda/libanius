/*
 * Libanius
 * Copyright (C) 2012-2019 James McCabe <james@oranda.com>
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

import com.oranda.libanius.model.quizitem.TextValueOps.TextValue

import scala.language.postfixOps
import scala.util.Random

import com.oranda.libanius.model.quizitem.QuizItem
import com.oranda.libanius.dependencies.AppDependencyAccess

import scalaz._
import scalaz.std.set
import scalaz.Scalaz._
import com.oranda.libanius.model._
import scala._
import scala.collection.immutable.Stream
import scala.collection.immutable.List
import scala.collection.immutable.Iterable
import scalaz.PLens._
import com.oranda.libanius.model.UserResponse

/*
 * QuizGroupMemoryLevel: a partition of a QuizGroup, containing quiz items which have
 * all been answered correctly a certain number of times.
 * (A ListMap was formerly used to store quiz items but this was found to be too
 * slow for bulk insert. Currently a Stream is used.)
 */
case class QuizGroupMemoryLevel(
    correctResponsesInARow: Int,
    repetitionInterval: Int,
    quizItemStream: Stream[QuizItem] = Stream.empty,
    totalResponses: Int = 0,
    numCorrectResponses: Int = 0)
  extends ModelComponent {

  lazy val quizItems = quizItemStream.toList
  protected[model] def numQuizItems = quizItems.size
  protected[model] def size = numQuizItems
  protected[model] def isEmpty = quizItems.isEmpty
  protected[model] def numPrompts = size
  protected[model] def numResponses = size

  protected[model] def contains(quizItem: QuizItem): Boolean =
    quizItems.exists(_.samePromptAndResponse(quizItem))

  protected[model] def contains(prompt: TextValue): Boolean =
      quizItems.exists(_.prompt == prompt)

  protected[model] def updatedQuizItems(newQuizItems: List[QuizItem]): QuizGroupMemoryLevel =
    QuizGroupMemoryLevel.itemsLens.set(this, newQuizItems)

  protected[model] def updatedWithUserAnswer(
      prompt: TextValue,
      response: TextValue,
      wasCorrect: Boolean,
      userResponses: UserResponsesAll,
      userAnswer: UserResponse): QuizGroupMemoryLevel = {
    val userResponseUpdated = userResponses.add(userAnswer, wasCorrect)
    this + QuizItem(prompt, response, userResponseUpdated)
  }

  protected[quizgroup] def addNewQuizItem(prompt: String, response: String): QuizGroupMemoryLevel =
    if (!prompt.isEmpty && !response.isEmpty && prompt.toLowerCase != response.toLowerCase)
      this + QuizItem(prompt, response)
    else
      this

  // Adds the quiz item to the front of the queue
  protected[quizgroup] def +(quizItem: QuizItem): QuizGroupMemoryLevel =
    QuizGroupMemoryLevel.itemsLens.set(this, quizItem +: remove(quizItem))

  protected[model] def -(quizItem: QuizItem): QuizGroupMemoryLevel =
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
    quizItems.filter(_.prompt.matches(prompt)).map(_.correctResponse.value)

  /*
   * Low usage expected. Slow because we are not using a Map for quizItems.
   */
  protected[quizgroup] def findPromptsFor(response: String): List[String] =
    quizItems.filter(_.correctResponse.matches(response)).map(_.prompt.value)

  /*
   * Low usage expected. Slow because we are not using a Map for quizItems.
   */
  protected[quizgroup] def findQuizItem(prompt: String, response: String): Option[QuizItem] =
    quizItems.find(_.samePromptAndResponse(QuizItem(prompt, response)))

  protected[quizgroup] def randomValues(sliceSize: Int): List[TextValue] =
    randomSliceOfQuizItems(sliceSize).map(_.correctResponse).toList

  protected[quizgroup] def randomSliceOfQuizItems(sliceSize: Int): Iterable[QuizItem] =
    if (sliceSize >= size) quizItems
    else {
      val randomStart = Random.nextInt(size - sliceSize)
      quizItems.slice(randomStart, randomStart + sliceSize)
    }

  override def toString = {
    val quizItemPrompts = quizItems.map(_.prompt)
    val averageCorrectResponses = numCorrectResponses/totalResponses
    s"$correctResponsesInARow($repetitionInterval):$averageCorrectResponses: $quizItemPrompts"
  }


  protected[model] def isAtLimit = totalResponses >= QuizGroupMemoryLevel.totalResponsesLimit

  protected[model] def inc(isCorrect: Boolean) = {
    val numCorrectToAdd = if (isCorrect) 1 else 0
    // For each memory level, only check the recent performance. Reset the counters after a limit.
    if (!isAtLimit)
      copy(totalResponses = totalResponses + 1,
          numCorrectResponses = numCorrectResponses + numCorrectToAdd)
    else // reset the counters
      copy(totalResponses = 1, numCorrectResponses = numCorrectToAdd)
  }

  protected[model] def updatedInterval: Int = {
    def modifyBy(anInt: Int, aReal: Double) =
      if (totalResponses <= 5) repetitionInterval + anInt
      else (repetitionInterval * (1 + aReal)).toInt

    math.max(0,
      if (numCorrectResponses < 7) modifyBy(-1, -0.2)
      else if (numCorrectResponses > 8)  modifyBy(1, 0.2)
      else repetitionInterval
    )
  }
}

object QuizGroupMemoryLevel extends AppDependencyAccess {

  val itemsLens: Lens[QuizGroupMemoryLevel, List[QuizItem]] = Lens.lensu(
    get = (_: QuizGroupMemoryLevel).quizItems,
    set = (qgp: QuizGroupMemoryLevel,
      qItems: List[QuizItem]) => qgp.copy(quizItemStream = qItems.toStream))

  val totalResponsesLimit = 10

  val intervalLens: Lens[QuizGroupMemoryLevel, Int] = Lens.lensu(
    get = (_: QuizGroupMemoryLevel).repetitionInterval,
    set = (qgml: QuizGroupMemoryLevel, ri: Int) => qgml.copy(repetitionInterval = ri))
}
