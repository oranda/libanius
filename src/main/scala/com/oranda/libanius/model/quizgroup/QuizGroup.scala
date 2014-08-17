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
import com.oranda.libanius.model.quizitem.{QuizItemViewWithChoices, QuizItem, TextValue}
import com.oranda.libanius.dependencies.AppDependencyAccess

import scalaz._
import com.oranda.libanius.model._
import scalaz.PLens._
import scala.collection.immutable.{Stream, List}
import scala.Predef._
import com.oranda.libanius.util.Util

/*
 * Contains quiz items for a topic.
 * For performance reasons, the quiz items are partitioned into "memory levels"
 * corresponding to how many times as a user has answered each item correctly. (This
 * is an example of a "disjoint set" or "union find" data structure.) The index in the
 * List of partitions matches correctResponseInARow.
 */
case class QuizGroup private(levels: List[QuizGroupMemoryLevel],
    userData: QuizGroupUserData, dictionary: Dictionary)
  extends ModelComponent {

  lazy val currentPromptNumber = userData.currentPromptNumber
  lazy val isActive = userData.isActive

  def updatedPromptNumber: QuizGroup = QuizGroup.promptNumberLens.mod((1+), this)

  def activate = QuizGroup.activeLens.set(this, true)
  def deactivate = QuizGroup.activeLens.set(this, false)

  def contains(quizItem: QuizItem): Boolean = levels.exists(_.contains(quizItem))
  def hasPrompt(prompt: String): Boolean = contains(prompt)
  def contains(prompt: String): Boolean = contains(TextValue(prompt))
  def contains(prompt: TextValue): Boolean = levels.exists(_.contains(prompt))
  def numQuizItems = (0 /: levels)(_ + _.numQuizItems)

  def isEmpty = levels.forall(_.isEmpty)
  def size = numQuizItems
  def numPrompts = size
  def numResponses = size
  def numLevels = levels.size
  def numCorrectResponsesRequired = numLevels - 1

  // Because memLevel index corresponds to numCorrectAnswersInARow, score computation is fast
  def numCorrectResponses: Int = levels.zipWithIndex.foldLeft(0) {
      case (accum, (level, index)) => accum + level.size * index
    }

  def maxDiffInPromptNumMinimum = (0 /: levels)(_ max _.repetitionInterval)

  def totalCorrectResponsesRequired = numQuizItems * numCorrectResponsesRequired

  def totalResponses(level: Int): Int = get(level).totalResponses
  def numCorrectResponses(level: Int): Int = get(level).numCorrectResponses
  def memoryLevelInterval(level: Int) = get(level).repetitionInterval
  def isAtLimit(level: Int): Boolean = get(level).isAtLimit

  def updatedDictionary(newDictionary: Dictionary) =
    QuizGroup.dictionaryLens.set(this, newDictionary)

  def numDictionaryKeyWords = dictionary.numKeyWords

  def quizItems: Stream[QuizItem] = levels.flatMap(_.quizItems).toStream

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

  protected[model] def findQuizItem(prompt: String): Option[QuizItem] =
    quizItems.find(quizItem => quizItem.prompt.matches(prompt))

  protected[model] def updateWithQuizItem(quizItem: QuizItem, isCorrect: Boolean,
      prevMemLevel: Int): QuizGroup = {
    val updatedQuizGroup1 = moveQuizItem(quizItem)
    val updatedQuizGroup2 = updatedQuizGroup1.incrementResponses(prevMemLevel, isCorrect)
    val updatedQuizGroup3 = updatedQuizGroup2.updatedPromptNumber
    updatedQuizGroup3.updateIntervalForLevel(prevMemLevel)
  }

  private def moveQuizItem(quizItem: QuizItem): QuizGroup = {
    val qgUpdated = removeQuizItem(quizItem)
    qgUpdated.prependItemToNthLevel(quizItem, quizItem.numCorrectResponsesInARow)
  }

  protected[model] def addNewQuizItem(prompt: String, response: String): QuizGroup =
    if (!prompt.isEmpty && !response.isEmpty && prompt.toLowerCase != response.toLowerCase)
      prependItemToFirstLevel(QuizItem(prompt, response))
    else
      this

  private def prependItemToFirstLevel(quizItem: QuizItem): QuizGroup =
    prependItemToNthLevel(quizItem, 0)

  private def prependItemToNthLevel(quizItem: QuizItem, n: Int): QuizGroup = {
    val nthLevelLens = listNthPLens(n) compose QuizGroup.levelsLens.partial
    val nthLevelItemsLens = QuizGroupMemoryLevel.itemsLens.partial compose nthLevelLens
    nthLevelItemsLens.mod(quizItem +: _.filterNot(_ == quizItem), this)
  }

  protected[model] def removeQuizItem(quizItem: QuizItem) =
    QuizGroup.levelsLens.set(this, levels.map(_.removeQuizItem(quizItem)))

  /*
   * This may give similar results to findResponsesFor but it uses the dictionary
   * and does not take into account any edits to the quiz group during a quiz run.
   */
  protected[model] def findValuesFor(prompt: String): List[String] =
    dictionary.findValuesFor(prompt).values.map(_.value)

  protected[model] def constructWrongChoices(itemCorrect: QuizItem,
      numCorrectResponsesSoFar: Int, numWrongChoicesRequired: Int = 2): List[String] = {

    val correctResponses = Util.stopwatch(findResponsesFor(itemCorrect.prompt.value),
        "findResponsesFor")
    val falseResponses: List[String] =
        constructWrongChoicesSimilar(numCorrectResponsesSoFar,
            itemCorrect, numWrongChoicesRequired, correctResponses) ++
        constructWrongChoicesRandom(correctResponses, numWrongChoicesRequired, itemCorrect) ++
        constructWrongChoicesDummy(numWrongChoicesRequired)
    falseResponses.distinct.take(numWrongChoicesRequired)
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
      levels.flatMap( _.constructWrongChoicesSimilar(correctResponses,
          numWrongChoicesRequired, correctValuePresented, similarityPredicate))
    }

  protected[quizgroup] def constructWrongChoicesRandom(correctResponses: List[String],
      numWrongChoicesRequired: Int, itemCorrect: QuizItem): List[String] =
    levels.filterNot(_.isEmpty).flatMap(_.constructWrongChoicesRandom(
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

  /*
   * The memLevels are searched in reverse order for presentable quiz items,
   * meaning that an item that has been answered correctly (once or more) will
   * be preferred over an item with no correct answers, assuming the
   * interval criteria (difference with the prompt number) is satisfied.
   */
  def findPresentableQuizItem: Option[QuizItem] =
    (for {
      (memLevel, levelIndex) <- levels.zipWithIndex.reverse.tail.toStream
      quizItem <- memLevel.findPresentableQuizItem(currentPromptNumber).toStream
    } yield quizItem).headOption


  protected[model] def findAnyUnfinishedQuizItem: Option[QuizItem] =
    (for {
      (memLevel, levelIndex) <- levels.zipWithIndex.reverse.tail.toStream
      quizItem <- memLevel.findAnyUnfinishedQuizItem
    } yield quizItem).headOption

  def incrementResponsesCorrect(memoryLevel: Int) = incrementResponses(memoryLevel, true)
  def incrementResponsesIncorrect(memoryLevel: Int) = incrementResponses(memoryLevel, false)

  def incrementResponses(memoryLevel: Int, isCorrect: Boolean): QuizGroup =
    QuizGroup.levelsListLens(memoryLevel).mod((_: QuizGroupMemoryLevel).inc(isCorrect), this)

  def updateIntervalForLevel(memoryLevel: Int): QuizGroup =
    if (memoryLevel > 0 && isAtLimit(memoryLevel)) {
      val mlsIntervalLens = QuizGroupMemoryLevel.intervalLens.partial compose
          QuizGroup.levelsListLens(memoryLevel)
      val mlsUpdated = mlsIntervalLens.mod(Int => get(memoryLevel).updatedInterval, this)
      def strIntervals(qg: QuizGroup) = qg.levels.map(_.repetitionInterval)
      l.log("Updating memory intervals from " + strIntervals(this) + " to " +
          strIntervals(mlsUpdated) + ", numCorrectResponses: " + numCorrectResponses(memoryLevel))
      mlsUpdated
    } else
      this

  protected[model] def get(level: Int): QuizGroupMemoryLevel = levels(level)


  protected[model] def quizItemWithChoices(quizItem: QuizItem, header: QuizGroupHeader):
      QuizItemViewWithChoices = {
    val numCorrectResponses = quizItem.userResponses.numCorrectResponsesInARow
    /*
     * A quiz item might be presented initially in multiple-choice format,
     * then later wihout any such assistance.
     */
    val useMultipleChoice = numCorrectResponses < header.useMultipleChoiceUntil
    val falseAnswers =
      if (useMultipleChoice)
        Util.stopwatch(constructWrongChoices(quizItem, numCorrectResponses),
          "constructWrongChoices")
      else Nil
    val numCorrectResponsesRequired = numLevels
    new QuizItemViewWithChoices(quizItem, currentPromptNumber, header,
        falseAnswers, numCorrectResponses, numCorrectResponsesRequired,
        useMultipleChoice)
  }
}

import com.oranda.libanius.model.UserResponse
object QuizGroup extends AppDependencyAccess {

  //def apply(quizItems: Stream[QuizItem] = Stream.empty): QuizGroup  =
  //  apply(quizItems, new QuizGroupUserData(true), new Dictionary())

  /*
   * Form a QuizGroup from quiz items with no user responses.
   */
  def fromQuizItems(quizItems: Stream[QuizItem] = Stream.empty,
      userData: QuizGroupUserData = QuizGroupUserData(true),
      dictionary: Dictionary = new Dictionary()): QuizGroup =
    QuizGroup(Map(0 -> QuizGroupMemoryLevel(0, 0, quizItems)), userData, dictionary)

  def apply(memLevelMap: Map[Int, QuizGroupMemoryLevel] = Map(),
      userData: QuizGroupUserData = QuizGroupUserData(true),
      dictionary: Dictionary = new Dictionary()): QuizGroup =
    QuizGroup(toMemLevelList(memLevelMap), userData, dictionary)

  def createFromMemLevels(memLevelMap: Map[Int, QuizGroupMemoryLevel],
      userData: QuizGroupUserData): QuizGroup =
    QuizGroup(toMemLevelList(memLevelMap), userData, new Dictionary())

  private def toMemLevelList(memLevelMap: Map[Int, QuizGroupMemoryLevel]):
      List[QuizGroupMemoryLevel] = {
    // Note: the final "level" is just a resting place for complete items
    val defaultIntervalList = List(0, 5, 15, 15, 60, 600, 0)
    defaultIntervalList.zipWithIndex.map {
      case (level, index) => memLevelMap.get(index).getOrElse(QuizGroupMemoryLevel(index, level))
    }
  }

  val levelsLens: Lens[QuizGroup, List[QuizGroupMemoryLevel]] = Lens.lensu(
    get = (_: QuizGroup).levels,
    set = (qGroup: QuizGroup,
        qLevels: List[QuizGroupMemoryLevel]) => qGroup.copy(levels = qLevels))

  protected[quizgroup] def levelsListLens(memoryLevel: Int) =
    listNthPLens(memoryLevel) compose levelsLens.partial

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
}


// simple correctResponse object used for saving a data structure to a file
case class SaveData(fileName: String, data: String)