/*
 * Libanius
 * Copyright (C) 2012-2020 James McCabe <james@oranda.com>
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

package com.oranda.libanius.model

import com.oranda.libanius.model.action.serialize.CustomFormatParserFast._
import com.oranda.libanius.model.quizitem.TextValueOps.TextValue
import fastparse.core.Parsed

import scala.collection.immutable._

import scala.language.postfixOps
import scala.math.BigDecimal.double2bigDecimal
import com.oranda.libanius.dependencies._
import com.oranda.libanius.model.quizitem.QuizItem
import com.oranda.libanius.model.wordmapping.Dictionary

import scalaz._
import PLens._
import com.oranda.libanius.model.quizgroup.{QuizGroup, QuizGroupHeader, QuizGroupWithHeader, WordMapping}
import scala.collection.immutable.Nil
import scala.collection.immutable.List
import scala.collection.immutable.Iterable
import com.oranda.libanius.net.providers.MyMemoryTranslate
import scala.util.Try

case class Quiz(private val quizGroups: Map[QuizGroupHeader, QuizGroup] = ListMap())
  extends ModelComponent {

  def quizGroupIterator = quizGroups.valuesIterator

  def hasQuizGroup(header: QuizGroupHeader): Boolean = quizGroups.contains(header)
  def isActive(header: QuizGroupHeader): Boolean = quizGroups.get(header).exists(_.isActive)

  // By default, reading of data accesses the activeQuizGroups
  def activeQuizGroups: Map[QuizGroupHeader, QuizGroup] =
    quizGroups.filter { case (_, quizGroup) => quizGroup.isActive }
  def activeQuizGroupHeaders: Set[QuizGroupHeader] = activeQuizGroups.keySet

  def numActiveGroups = activeQuizGroups.size
  def numPrompts = activeQuizGroups.values.view.map(_.numPrompts).sum
  def numResponses = activeQuizGroups.values.view.map(_.numResponses).sum
  def numQuizItems = activeQuizGroups.values.view.map(_.size).sum
  def numCorrectResponses = activeQuizGroups.values.view.map(_.numCorrectResponses).sum
  def totalCorrectResponsesRequired =
    activeQuizGroups.values.view.map(_.totalCorrectResponsesRequired).sum
  def scoreSoFar: BigDecimal =  // out of 1.0
    numCorrectResponses.toDouble / totalCorrectResponsesRequired.toDouble

  def numCorrectResponses(qgh: QuizGroupHeader, level: Int) =
    quizGroups(qgh).numCorrectResponses(level)

  def memoryLevelInterval(qgh: QuizGroupHeader, level: Int) =
    quizGroups(qgh).memoryLevelInterval(level)

  def numDictionaryKeyWords(qgh: QuizGroupHeader) = quizGroups(qgh).numDictionaryKeyWords

  def resultsBeginningWith(input: String): List[SearchResult] =
    activeQuizGroups.flatMap {
      case (header, quizGroup) =>
        Dictionary.convertToSearchResults(
            quizGroup.dictionary.mappingsForKeysBeginningWith(input),
            header)
    }.toList

  def resultsContaining(input: String): List[SearchResult] =
    activeQuizGroups.flatMap {
      case (header, quizGroup) =>
        Dictionary.convertToSearchResults(
        quizGroup.dictionary.mappingsForKeysContaining(input),
        header)
    }.toList

  def isCorrect(quizGroupHeader: QuizGroupHeader, prompt: String, userResponse: String):
      Boolean = {
    val responses = findResponsesFor(prompt, quizGroupHeader)
    responses.exists(_.looselyMatches(userResponse))
  }

  /*
   * Do not call in a loop: not fast.
   */
  def findResponsesFor(prompt: String, header: QuizGroupHeader): List[String] =
    activeQuizGroups.get(header).map(_.findResponsesFor(prompt)).getOrElse(Nil)

  /*
   *  Do not call in a loop: not fast.
   */
  def findPromptsFor(response: String, header: QuizGroupHeader): List[String] =
    activeQuizGroups.get(header).map(_.findPromptsFor(response)).getOrElse(Nil)

  def updatedQuizGroups(quizGroups: Map[QuizGroupHeader, QuizGroup]): Quiz =
    Quiz.quizGroupsLens.set(this, quizGroups)

  def activate(header: QuizGroupHeader): Quiz =
    Quiz.quizGroupsLens.set(this, mapVPLens(header) mod ((_: QuizGroup).activate, quizGroups))

  def deactivate(header: QuizGroupHeader): Quiz =
    Quiz.quizGroupsLens.set(this, mapVPLens(header) mod ((_: QuizGroup).deactivate, quizGroups))

  /*
   * Will not replace existing quiz groups.
   */
  def addQuizGroups(qgwhs: Iterable[QuizGroupWithHeader]): Quiz =
    addQuizGroups(qgwhs.map(_.toPair).toMap)

  /*
   * Will not replace existing quiz groups.
   */
  def addQuizGroups(qgs: Map[QuizGroupHeader, QuizGroup]): Quiz = {
    val newQuizGroups = qgs -- quizGroups.keySet
    Quiz.quizGroupsLens.set(this, quizGroups ++ newQuizGroups)
  }

  /*
   * Will not replace an existing quiz group.
   */
  def addQuizGroup(header: QuizGroupHeader, quizGroup: QuizGroup): Quiz =
    if (!hasQuizGroup(header)) setQuizGroup(header, quizGroup) else this

  /*
   * Add or replace any existing quiz group with the given header.
   */
  def addOrReplaceQuizGroup(header: QuizGroupHeader, quizGroup: QuizGroup): Quiz =
    setQuizGroup(header, quizGroup)

  private def setQuizGroup(header: QuizGroupHeader, quizGroup: QuizGroup): Quiz =
    Quiz.quizGroupsLens.set(this, quizGroups + (header -> quizGroup))

  // adds the quiz item to the front of the right queue within the Quiz data structure
  def addQuizItemToFront(quizItem: QuizItem, header: QuizGroupHeader): Quiz =
    Quiz.quizGroupsLens.set(this,
      mapVPLens(header) mod ((_: QuizGroup) + quizItem, quizGroups)
    )

  /*
   * This is intended for reversible quiz items, principally word translations.
   */
  def addQuizItemToFrontOfTwoGroups(quizItem: QuizItem, header: QuizGroupHeader): Quiz =
    addQuizItemToFront(quizItem, header).addQuizItemToFront(quizItem, header.reverse)

  def removeQuizItem(prompt: String, response: String, header: QuizGroupHeader): (Quiz, Boolean) =
    removeQuizItem(QuizItem(prompt, response), header)

  def existsQuizItem(quizItem: QuizItem, header: QuizGroupHeader) =
    mapVPLens(header).get(activeQuizGroups).map(_.contains(quizItem)).isDefined

  def removeQuizItem(quizItem: QuizItem, header: QuizGroupHeader): (Quiz, Boolean) = {
    val quizItemExisted = existsQuizItem(quizItem, header)
    val quiz: Quiz = Quiz.quizGroupsLens.set(this,
        mapVPLens(header) mod ((_: QuizGroup) - quizItem, quizGroups))
    (quiz, quizItemExisted)
  }

  def qgCurrentPromptNumber(header: QuizGroupHeader): Option[Int] =
    mapVPLens(header).get(activeQuizGroups).map(_.currentPromptNumber)

  def findQuizItem(header: QuizGroupHeader, prompt: String): Option[QuizItem] =
    mapVPLens(header).get(activeQuizGroups).flatMap(_.findQuizItem(prompt))

  def findQuizItem(header: QuizGroupHeader, prompt: String, response: String):
      Option[QuizItem] =
    mapVPLens(header).get(activeQuizGroups).flatMap(_.findQuizItem(prompt, response))

  def updateWithUserResponse(isCorrect: Boolean, quizGroupHeader: QuizGroupHeader,
      quizItem: QuizItem): Quiz =
    qgCurrentPromptNumber(quizGroupHeader) match {
      case Some(qgPromptNumber) =>
        val userResponse = new UserResponse(qgPromptNumber)

        val prevMemLevel = quizItem.numCorrectResponsesInARow
        val updQuizItem = quizItem.updatedWithUserResponse(quizItem.correctResponse,
            isCorrect, userResponse)

        Quiz.quizGroupsLens.set(this, mapVPLens(quizGroupHeader) mod
            ((_: QuizGroup).updateWithQuizItem(updQuizItem, isCorrect, prevMemLevel), quizGroups)
        )
      case _ => this
    }

  def nearTheEnd = quizGroupIterator.exists(qg =>
      (qg.numPrompts - qg.currentPromptNumber) < qg.maxDiffInPromptNumMinimum)

  def searchLocalDictionary(searchInput: String): Try[List[SearchResult]] = {
    import Dictionary._  // make special search utilities available

    val firstWord = searchInput.takeWhile(_ != ' ')

    // Keep trying different ways of searching the dictionary until one finds something.
    Try(
      if (firstWord.length <= 2) Nil
      else tryUntilResults(List(
        searchFunction { resultsBeginningWith(firstWord) },
        searchFunction { resultsBeginningWith(firstWord.dropRight(1)) },
        searchFunction { resultsBeginningWith(firstWord.dropRight(2)) },
        searchFunction { if (firstWord.length > 3) resultsContaining(firstWord) else Nil }
      ))
    )
  }

  def searchRemoteDictionary(searchInput: String): Try[List[SearchResult]] =
    MyMemoryTranslate.translate(searchInput, this)
}

object Quiz extends AppDependencyAccess {

  val quizGroupsLens: Lens[Quiz, Map[QuizGroupHeader, QuizGroup]] = Lens.lensu(
    get = (_: Quiz).quizGroups,
    set = (q: Quiz, qgs: Map[QuizGroupHeader, QuizGroup]) => q.copy(quizGroups = qgs))

  def quizGroupMapLens[QuizGroupHeader, QuizGroup](header: QuizGroupHeader):
      Lens[Map[QuizGroupHeader, QuizGroup], Option[QuizGroup]] =
    Lens.lensu(
      get = _ get header,
      set = (quizGroups, qg) => qg match {
        case None => quizGroups - header
        case Some(quizGroup) => quizGroups + ((header, quizGroup))
      })

  def metadataFromCustomFormat(str: String): Set[QuizGroupHeader] = {
    val quizGroupHeadings = str.split("quizGroup").tail
    quizGroupHeadings.map(QuizGroupHeader(_)).toSet
  }

  def getDefaultQuiz = getQuizWithOneGroup(conf.defaultPromptType, conf.defaultResponseType)

  def getQuizWithOneGroup(promptType: String, responseType: String) =
    dataStore.findQuizGroupHeader(promptType, responseType, WordMapping) match {
      case Some(initQgh) =>
        val quizGroup = dataStore.initQuizGroup(initQgh)
        Quiz(Map(initQgh -> quizGroup))
      case _ => Quiz.demoQuiz()
    }

  def demoQuiz(quizGroupsData: List[String] = demoDataInCustomFormat): Quiz = {
    l.log("Using demo data")
    val qgsWithHeader: Iterable[QuizGroupWithHeader] = quizGroupsData.map { qgwh =>
      val Parsed.Success(qgh, _) = quizGroupWithHeader.parse(qgwh)
      qgh
    }
    Quiz(qgsWithHeader.map(qgWithHeader => (qgWithHeader.header, qgWithHeader.quizGroup)).toMap)
  }

  val memLevelsWithLowIntervals =
    """#quizGroupPartition numCorrectResponsesInARow="1" repetitionInterval="2"
      |#quizGroupPartition numCorrectResponsesInARow="2" repetitionInterval="2"
      |#quizGroupPartition numCorrectResponsesInARow="3" repetitionInterval="2"
      |#quizGroupPartition numCorrectResponsesInARow="4" repetitionInterval="2"
      |#quizGroupPartition numCorrectResponsesInARow="5" repetitionInterval="2"
      |#quizGroupPartition numCorrectResponsesInARow="6" repetitionInterval="2"""".stripMargin

  // Demo data to use as a fallback if no file is available
  def demoDataInCustomFormat = List(

    s"""#quizGroup type="WordMapping" promptType="English word" responseType="German word" isActive="true" currentPromptNumber="0"
      |#quizGroupPartition numCorrectResponsesInARow="0" repetitionInterval="0"
      |en route|unterwegs
      |contract|Vertrag
      |treaty|Vertrag
      |against|wider
      |entertain|unterhalten
      |$memLevelsWithLowIntervals""".stripMargin
      ,

    s"""#quizGroup type="WordMapping" promptType="German word" responseType="English word" isActive="true" currentPromptNumber="0"
      |#quizGroupPartition numCorrectResponsesInARow="0" repetitionInterval="0"
      |unterwegs|en route
      |Vertrag|contract
      |Vertrag|treaty
      |wider|against
      |unterhalten|entertain
      |$memLevelsWithLowIntervals""".stripMargin

  )

}
