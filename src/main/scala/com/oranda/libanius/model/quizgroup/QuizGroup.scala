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

import scala.language.postfixOps

import com.oranda.libanius.model.wordmapping._
import com.oranda.libanius.model.quizitem.{QuizItem, TextValue}
import com.oranda.libanius.dependencies.AppDependencyAccess

import scalaz._
import com.oranda.libanius.model.{Criteria, UserResponses, ModelComponent, UserResponse}
import java.lang.StringBuilder
import scalaz.PLens._
import scala.collection.immutable.Stream
import scala.collection.immutable.List
import scala.Predef._
import scala.collection.immutable.Set
import com.oranda.libanius.util.{StringUtil, Util}

/*
 * Contains quiz items for a topic.
 * For performance reasons, the quiz items are partitioned according to how
 * many times as a user has answered each item correctly. (This is an example
 * of a "disjoint set" or "union find" data structure.) The index in the ArrayList
 * of partitions matches correctResponseInARow.
 */
case class QuizGroup private(partitions: List[QuizGroupPartition] = List(),
    userData: QuizGroupUserData = QuizGroupUserData(),
    dictionary: Dictionary = new Dictionary)
  extends ModelComponent {

  lazy val currentPromptNumber = userData.currentPromptNumber
  lazy val isActive = userData.isActive

  def updatedPromptNumber: QuizGroup = QuizGroup.promptNumberLens.mod((1+), this)

  def activate = QuizGroup.activeLens.set(this, true)
  def deactivate = QuizGroup.activeLens.set(this, false)

  def contains(quizItem: QuizItem): Boolean = partitions.exists(_.contains(quizItem))
  def hasPrompt(prompt: String): Boolean = contains(prompt)
  def contains(prompt: String): Boolean = contains(TextValue(prompt))
  def contains(prompt: TextValue): Boolean = partitions.exists(_.contains(prompt))
  def numQuizItems = (0 /: partitions)(_ + _.numQuizItems)

  def isEmpty = partitions.forall(_.isEmpty)
  def size = numQuizItems
  def numPrompts = size
  def numResponses = size

  // Because partition index corresponds to numCorrectAnswersInARow, score computation is fast
  def numCorrectAnswers: Int = partitions.zipWithIndex.foldLeft(0) {
      case (accum, (partition, index)) => accum + partition.size * index
  }

  def updatedDictionary(newDictionary: Dictionary) =
    QuizGroup.dictionaryLens.set(this, newDictionary)

  def quizItems: Stream[QuizItem] = partitions.flatMap(_.quizItems).toStream

  protected[model] def updatedWithUserResponse(prompt: TextValue, response: TextValue,
      wasCorrect: Boolean, userResponses: UserResponses, userResponse: UserResponse):
      QuizGroup = {
    val userResponsesUpdated = userResponses.add(userResponse, wasCorrect)
    moveQuizItem(QuizItem(prompt, response, userResponsesUpdated))
  }

  /*
   * Low usage expected. Slow because we are not using a Map for quizItems.
   */
  protected[model] def findResponsesFor(prompt: String): List[String] =
    quizItems.filter(_.prompt.matches(prompt)).map(_.correctResponse.value).toList

  /*
   * Low usage expected. Slow because we are not using a Map for quizItems.
   */
  protected[model] def findPromptsFor(response: String): List[String] =
    quizItems.filter(_.correctResponse.matches(response)).map(_.prompt.value).toList

  /*
   * Low usage expected. Slow because we are not using a Map for quizItems.
   */
  protected[model] def findQuizItem(prompt: String, response: String): Option[QuizItem] =
    quizItems.find(quizItem => quizItem.prompt.matches(prompt) &&
        quizItem.correctResponse.matches(response))

  protected[model] def moveQuizItem(quizItem: QuizItem): QuizGroup = {
    val qgUpdated = removeQuizItem(quizItem)
    qgUpdated.prependItemToNthPartition(quizItem, quizItem.numCorrectAnswersInARow)
  }

  protected[model] def addNewQuizItem(prompt: String, response: String): QuizGroup =
    if (!prompt.isEmpty && !response.isEmpty && prompt.toLowerCase != response.toLowerCase)
      //addQuizItemToFront(QuizItem(prompt, response))
      prependItemToFirstPartition(QuizItem(prompt, response))
    else
      this

  private def prependItemToFirstPartition(quizItem: QuizItem): QuizGroup =
    prependItemToNthPartition(quizItem, 0)

  private def prependItemToNthPartition(quizItem: QuizItem, n: Int): QuizGroup = {
    val firstPartitionLens = listNthPLens(n) compose QuizGroup.partitionsLens.partial
    val firstPartitionItemsLens = QuizGroupPartition.itemsLens.partial compose firstPartitionLens
    firstPartitionItemsLens.mod(quizItem +: _.filterNot(_ == quizItem), this)
  }

  protected[model] def removeQuizItem(quizItem: QuizItem) =
    QuizGroup.partitionsLens.set(this, partitions.map(_.removeQuizItem(quizItem)))

  /*
   * This may give similar results to findResponsesFor but it uses the dictionary
   * and does not take into account any edits to the quiz group during a quiz run.
   */
  protected[model] def findValuesFor(prompt: String): List[String] =
    dictionary.findValuesFor(prompt).values.map(_.value)

  protected[model] def constructWrongChoices(itemCorrect: QuizItem,
      numCorrectResponsesSoFar: Int, numWrongChoicesRequired: Int = 2): Set[String] = {

    val correctResponses = Util.stopwatch(findResponsesFor(itemCorrect.prompt.value),
        "findResponsesFor")
    val falseResponses: List[String] =
        constructWrongChoicesSimilar(numCorrectResponsesSoFar,
            itemCorrect, numWrongChoicesRequired, correctResponses) ++
        constructWrongChoicesRandom(correctResponses, numWrongChoicesRequired, itemCorrect) ++
        constructWrongChoicesDummy(numWrongChoicesRequired)
    falseResponses.take(numWrongChoicesRequired).toSet
  }

  /*
   * If the user has already been having success with this item, first try to
   * find responses that look similar to the correct one.
   */
  def constructWrongChoicesSimilar(numCorrectResponsesSoFar: Long, itemCorrect: QuizItem,
      numWrongChoicesRequired: Int, correctResponses: List[String]): List[String] =
    if (numCorrectResponsesSoFar == 0)
      Nil
    else {
      val correctValuePresented = itemCorrect.correctResponse.value

      def hasSameStart = (value1: TextValue, value2: String) => value1.hasSameStart(value2)
      def hasSameEnd = (value1: TextValue, value2: String) => value1.hasSameEnd(value2)
      val similarityPredicate = if (numCorrectResponsesSoFar % 2 == 1) hasSameStart else hasSameEnd
      partitions.flatMap( _.constructWrongChoicesSimilar(correctResponses,
          numWrongChoicesRequired, correctValuePresented, similarityPredicate))
    }

  protected[quizgroup] def constructWrongChoicesRandom(correctResponses: List[String],
      numWrongChoicesRequired: Int, itemCorrect: QuizItem): List[String] =
    partitions.filterNot(_.isEmpty).flatMap(_.constructWrongChoicesRandom(
        correctResponses, numWrongChoicesRequired, itemCorrect))

  protected[quizgroup] def constructWrongChoicesDummy(numWrongChoicesRequired: Int):
      List[String] = {
    val characters = "abcdefghijklmnopqrstuvwxyz0123456789".toCharArray
    if (numWrongChoicesRequired > characters.length) {
      l.logError("Too many dummy answers requested.")
      Nil
    } else
      characters.map(_.toString).take(numWrongChoicesRequired).toList
  }

  protected[model] def findAnyUnfinishedQuizItem: Option[QuizItem] =
    (for {
      partition <- partitions.reverse.toStream
      quizItem <- partition.findAnyUnfinishedQuizItem
    } yield quizItem).headOption


  /*
   * The partitions are searched in reverse order for presentable quiz items,
   * meaning that an item that has been answered correctly (once or more) will
   * be preferred over an item with no correct answers, assuming the
   * diffInPromptNumMinimum criteria is satisfied.
   */
  def findPresentableQuizItem: Option[QuizItem] =
    (for {
      partition <- partitions.reverse.toStream
      quizItem <- partition.findPresentableQuizItem(currentPromptNumber).toStream
    } yield quizItem).headOption


  /*
   * Example of custom format:
   *
   * quizGroup type="WordMapping" promptType="English word" responseType="German word" currentPromptNumber="0" isActive="true"
   *    against|wider
   *    entertain|unterhalten
   */
  def toCustomFormat(strBuilder: StringBuilder, header: QuizGroupHeader) = {
    userData.toCustomFormat(header.toCustomFormat(strBuilder))
    strBuilder.append('\n')
    for (partition <- partitions.zipWithIndex.toStream)
      partition._1.toCustomFormat(strBuilder, header.mainSeparator, partition._2)

    strBuilder
  }
}


object QuizGroup extends AppDependencyAccess {

  def apply(): QuizGroup = QuizGroup(List(), QuizGroupUserData(true), new Dictionary())

  def apply(quizItems: Stream[QuizItem]): QuizGroup  =
    apply(quizItems, new QuizGroupUserData(true), new Dictionary())

  /*
   * Form a QuizGroup from quiz items with no user responses.
   */
  def apply(quizItems: Stream[QuizItem], userData: QuizGroupUserData,
      dictionary: Dictionary): QuizGroup = {
    val partitions = Array.fill(Criteria.numCorrectAnswersRequired + 1)(QuizGroupPartition())
    partitions(0) = QuizGroupPartition(quizItems)
    QuizGroup(partitions.toList, userData, dictionary)
  }

  def createFromPartitions(partitionMap: Map[Int, QuizGroupPartition],
      userData: QuizGroupUserData): QuizGroup =
    QuizGroup(toPartitionArray(partitionMap), userData, new Dictionary())

  /*
   * Form a QuizGroup from quiz items with user responses.
   */
  def createFromQuizItems(quizItems: Stream[QuizItem], userData: QuizGroupUserData,
      header: String): QuizGroup = {

    val quizItemsGrouped: scala.collection.immutable.Map[Int, QuizGroupPartition] =
        quizItems.groupBy(_.numCorrectAnswersInARow).map {
          case (numCorrectAnswers: Int, quizItems: Stream[QuizItem]) =>
            (numCorrectAnswers, QuizGroupPartition(quizItems))
        }

    if (quizItemsGrouped.size > Criteria.numCorrectAnswersRequired + 1) {
      l.logError("Corrupt data for " + header +
          ": it looks like there is a quizItem with more than " +
          Criteria.numCorrectAnswersRequired + " correct responses stored")
      QuizGroup()

    } else {
      val partitions = toPartitionArray(quizItemsGrouped)
      val dictionary = Dictionary.fromQuizItems(quizItems)
      QuizGroup(partitions, userData, dictionary)
    }
  }

  def apply(partitions: Array[QuizGroupPartition], userData: QuizGroupUserData,
      dictionary: Dictionary): QuizGroup =
    QuizGroup(partitions.toList, userData, dictionary)

  def apply(partitions: List[QuizGroupPartition], userData: QuizGroupUserData): QuizGroup =
    QuizGroup(toPartitionArray(partitions), userData, new Dictionary())

  def apply(userData: QuizGroupUserData): QuizGroup =
    QuizGroup(List(), userData)

  def apply(partitions: List[QuizGroupPartition], dictionary: Dictionary): QuizGroup =
    QuizGroup(toPartitionArray(partitions), QuizGroupUserData(), dictionary)

  def toPartitionArray(partitions: List[QuizGroupPartition]): Array[QuizGroupPartition] = {
    val partitionArray = Array.fill(Criteria.numCorrectAnswersRequired + 1)(QuizGroupPartition())
    partitions.toArray.copyToArray(partitionArray)
    partitionArray
  }

  def toPartitionArray(partitionMap: Map[Int, QuizGroupPartition]): Array[QuizGroupPartition] = {
    val partitionArray = Array.fill(Criteria.numCorrectAnswersRequired + 1)(QuizGroupPartition())
    partitionMap.foreach {
      case (numCorrectAnswers: Int, partition: QuizGroupPartition) =>
        partitionArray(numCorrectAnswers) = partition
    }
    partitionArray
  }

  val partitionsLens: Lens[QuizGroup, List[QuizGroupPartition]] = Lens.lensu(
    get = (_: QuizGroup).partitions,
    set = (qGroup: QuizGroup,
           qPartitions: List[QuizGroupPartition]) => qGroup.copy(partitions = qPartitions))

  val quizGroupItemsLens: Lens[QuizGroup, Stream[QuizItem]] = Lens.lensu(
      get = (_: QuizGroup).quizItems,
      set = (qGroup: QuizGroup, qItems: Stream[QuizItem]) => qGroup/*qGroup.copy(quizItems = qItems)*/)

  val dictionaryLens: Lens[QuizGroup, Dictionary] = Lens.lensu(
      get = (_: QuizGroup).dictionary,
      set = (qGroup: QuizGroup, d: Dictionary) => qGroup.copy(dictionary = d))

  val userDataLens = Lens.lensu(
      get = (_: QuizGroup).userData,
      set = (qg: QuizGroup, ud: QuizGroupUserData) => qg.copy(userData = ud))

  val activeLens: Lens[QuizGroup, Boolean] =
    QuizGroupUserData.activeLens compose userDataLens

  val promptNumberLens: Lens[QuizGroup, Int] =
    QuizGroupUserData.promptNumberLens compose userDataLens

  def remove(quizItems: Stream[QuizItem], quizItem: QuizItem): Stream[QuizItem] =
    quizItems.filterNot(_.samePromptAndResponse(quizItem))


  /*
   * Text includes header line
   */
  def fromCustomFormat(text: String, mainSeparator: String, headerLine: String): QuizGroup = {

    val quizGroupParts = text.split("#quizGroupPartition ")
    val headerLine = quizGroupParts.head
    val quizGroupPartitions = quizGroupParts.tail

    def parsePartitionText(partitionText: String): Pair[Int, QuizGroupPartition] = {
      val headerLine = partitionText.takeWhile(_ != '\n')
      val mainPartitionText = partitionText.dropWhile(_ != '\n').tail
      val partition = QuizGroupPartition.fromCustomFormat(mainPartitionText, mainSeparator)
      val index = StringUtil.parseValue(headerLine,
          "numCorrectResponsesInARow=\"", "\"").getOrElse("0").toInt
      Pair(index, partition)
    }

    val partitionMap = quizGroupPartitions.map(parsePartitionText(_)).toMap
    val userData: QuizGroupUserData = QuizGroupUserData(headerLine)

    val quizGroup = QuizGroup.createFromPartitions(partitionMap, userData)
    quizGroup
  }
}


// simple correctResponse object used for saving a data structure to a file
case class SaveData(fileName: String, data: String)