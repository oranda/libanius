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

package com.oranda.libanius.model.quizgroup

import scala.collection.immutable._
import scala.language.postfixOps
import scala.util.{Random}

import com.oranda.libanius.model.quizitem.{QuizItem, TextValue}
import com.oranda.libanius.dependencies.AppDependencyAccess

import scalaz._
import scalaz.std.set
import scalaz.Scalaz._
import com.oranda.libanius.model.{UserResponses, ModelComponent}
import scala._
import com.oranda.libanius.model.UserResponse
import scala.collection.immutable.Stream
import scala.collection.immutable.Seq
import scala.collection.immutable.List
import scala.collection.immutable.Iterable

/*
 * A subgroup of a QuizGroup, containing quiz items which have all been answered
 * correctly a certain number of times.
 * (A ListMap was formerly used to store quiz items but this was found to be too
 * slow for bulk insert. Currently a Stream is used.)
 */
case class QuizGroupPartition(quizItems: Stream[QuizItem] = Stream.empty)
    extends ModelComponent {

  protected[quizgroup] def updatedQuizItems(newQuizItems: Stream[QuizItem]): QuizGroupPartition =
    QuizGroupPartition.itemsLens.set(this, newQuizItems)

  protected[quizgroup] def updatedWithUserAnswer(prompt: TextValue, response: TextValue, wasCorrect: Boolean,
      userResponses: UserResponses, userAnswer: UserResponse): QuizGroupPartition = {
    val userResponseUpdated = userResponses.add(userAnswer, wasCorrect)
    addQuizItem(prompt, response, userResponseUpdated)
  }

  protected[quizgroup] def addQuizItem(prompt: TextValue, response: TextValue,
      userResponses: UserResponses = UserResponses()): QuizGroupPartition =
    addQuizItemToFront(QuizItem(prompt, response, userResponses))

  protected[quizgroup] def addNewQuizItem(prompt: String, response: String): QuizGroupPartition =
    if (!prompt.isEmpty && !response.isEmpty && prompt.toLowerCase != response.toLowerCase)
      addQuizItemToFront(QuizItem(prompt, response))
    else
      this

  protected[quizgroup] def addQuizItemToFront(quizItem: QuizItem): QuizGroupPartition =
    addQuizItemToFront(quizItems, quizItem)

  protected[quizgroup] def addQuizItemToFront(quizItems: Stream[QuizItem],
      quizItem: QuizItem): QuizGroupPartition =
    QuizGroupPartition.itemsLens.set(this, quizItem +: remove(quizItem))

  protected[quizgroup] def addQuizItemToEnd(quizItem: QuizItem): QuizGroupPartition =
    addQuizItemToEnd(quizItems, quizItem)

  protected[quizgroup] def addQuizItemToEnd(quizItems: Stream[QuizItem], quizItem: QuizItem):
      QuizGroupPartition =
    updatedQuizItems(remove(quizItem) :+ quizItem)

  protected[quizgroup] def removeQuizItem(quizItem: QuizItem): QuizGroupPartition =
    QuizGroupPartition.itemsLens.set(this, remove(quizItem))

  protected[quizgroup] def remove(quizItem: QuizItem): Stream[QuizItem] =
    quizItems.filterNot(_.samePromptAndResponse(quizItem))

  protected[quizgroup] def removeQuizItemsForPrompt(prompt: String) =
    updatedQuizItems(quizItems.filter(_.prompt.value != prompt))
  protected[quizgroup] def removeQuizItemsForResponse(response: String) =
    updatedQuizItems(quizItems.filter(_.correctResponse.value != response))

  protected[quizgroup] def quizPrompts: Stream[TextValue] = quizItems.map(_.prompt)
  protected[quizgroup] def quizResponses: Stream[TextValue] = quizItems.map(_.correctResponse)


  protected[quizgroup] def contains(quizItem: QuizItem): Boolean =
    quizItems.exists(_.samePromptAndResponse(quizItem))
  protected[quizgroup] def contains(prompt: TextValue): Boolean =
    quizItems.exists(_.prompt == prompt)
  protected[quizgroup] def contains(prompt: String): Boolean = contains(TextValue(prompt))
  protected[quizgroup] def numQuizItems = quizItems.size
  protected[quizgroup] def size = numQuizItems
  protected[quizgroup] def isEmpty = quizItems.isEmpty
  protected[quizgroup] def numPrompts = size
  protected[quizgroup] def numResponses = size
  protected[quizgroup] def numCorrectAnswers: Int = (0 /: quizItems)(_ + _.numCorrectAnswersInARow)

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
    (for {
      quizItem <- quizItems.toStream
      if quizItem.userResponses.isUnfinished
     } yield quizItem).headOption

  def findPresentableQuizItem(currentPromptNumber: Int): Option[QuizItem] =
    (for {
      quizItem <- quizItems.toStream
      if quizItem.isPresentable(currentPromptNumber)
    } yield quizItem).headOption

  protected[quizgroup] def constructWrongChoicesSimilar(correctResponses: List[String],
      numWrongResponsesRequired: Int, correctValue: String,
      similarityPredicate: (TextValue, String) => Int => Boolean): Stream[String] = {

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
    similarWords.toStream
  }

  protected[quizgroup] def constructWrongChoicesRandom(correctValues: List[String],
      numFalseAnswersRequired: Int, itemCorrect: QuizItem): Stream[String] = {

    def randomFalseWordValue(sliceIndex: Int): Option[String] =
      findRandomWordValue(sliceOfResponses(sliceIndex, numSlices = numFalseAnswersRequired)).
          filter(!correctValues.contains(_))

    (0 until numFalseAnswersRequired).map(
        sliceIndex => randomFalseWordValue(sliceIndex)).flatten.toSet.toStream
  }

  protected[quizgroup] def findRandomWordValue(quizValues: Seq[TextValue]): Option[String] =
    !quizValues.isEmpty option quizValues(Random.nextInt(quizValues.length)).value

  protected[quizgroup] def sliceOfResponses(sliceIndex: Int, numSlices: Int): List[TextValue] =
    sliceOfQuizItems(sliceIndex, numSlices).map(_.correctResponse).toList

  protected[quizgroup] def sliceOfQuizItems(sliceIndex: Int, numSlices: Int): Iterable[QuizItem] = {
    val sliceSize = size / numSlices
    val start = sliceIndex * sliceSize
    quizItems.slice(start, start + sliceSize)
  }

  protected[quizgroup] def randomValues(sliceSize: Int): List[TextValue] =
    randomSliceOfQuizItems(sliceSize).map(_.correctResponse).toList

  protected[quizgroup] def randomSliceOfQuizItems(sliceSize: Int): Iterable[QuizItem] =
    if (sliceSize >= size) quizItems.toList
    else {
      val randomStart = Random.nextInt(size - sliceSize)
      quizItems.slice(randomStart, randomStart + sliceSize)
    }
}

object QuizGroupPartition extends AppDependencyAccess {

  val itemsLens: Lens[QuizGroupPartition, Stream[QuizItem]] = Lens.lensu(
      get = (_: QuizGroupPartition).quizItems,
      set = (qgp: QuizGroupPartition,
           qItems: Stream[QuizItem]) => qgp.copy(quizItems = qItems))

  def remove(quizItems: Stream[QuizItem], quizItem: QuizItem): Stream[QuizItem] =
    quizItems.filterNot(_.samePromptAndResponse(quizItem))
}