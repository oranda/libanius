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

package com.oranda.libanius.model

import scala.collection.immutable._

import scala.language.postfixOps
import scala.math.BigDecimal.double2bigDecimal
import com.oranda.libanius.dependencies._
import com.oranda.libanius.model.quizitem.{TextValue, QuizItem, QuizItemViewWithChoices}
import com.oranda.libanius.model.wordmapping.Dictionary

import scalaz._
import scalaz.std.set
import Scalaz._, PLens._
import com.oranda.libanius.model.quizgroup.{QuizGroupWithHeader, QuizGroupHeader, QuizGroup}

case class Quiz(private val quizGroups: Map[QuizGroupHeader, QuizGroup] = ListMap())
    extends ModelComponent {

  def hasQuizGroup(header: QuizGroupHeader): Boolean = quizGroups.contains(header)
  def isActive(header: QuizGroupHeader): Boolean = quizGroups.get(header).exists(_.isActive)

  // By default, reading of data accesses the activeQuizGroups
  def activeQuizGroups: Map[QuizGroupHeader, QuizGroup] = quizGroups.filter(_._2.isActive)
  def activeQuizGroupHeaders: Set[QuizGroupHeader] = activeQuizGroups.map(_._1).toSet

  def numActiveGroups = activeQuizGroups.size
  def numPrompts = (0 /: activeQuizGroups.values)(_ + _.numPrompts)
  def numResponses = (0 /: activeQuizGroups.values)(_ + _.numResponses)
  def numQuizItems = (0 /: activeQuizGroups.values)(_ + _.size)
  def numCorrectAnswers = (0 /: activeQuizGroups.values)(_ + _.numCorrectAnswers)
  def scoreSoFar: BigDecimal =  // out of 1.0
    numCorrectAnswers.toDouble / (numQuizItems * conf.numCorrectAnswersRequired).toDouble

  /*
   * Find the first available "presentable" quiz item.
   * Return a quiz item view and the associated quiz group header.
   */
  def findPresentableQuizItem: Option[(QuizItemViewWithChoices, QuizGroupWithHeader)] =
    (for {
      (header, quizGroup) <- activeQuizGroups
      quizItem <- QuizGroupWithHeader(header, quizGroup).findPresentableQuizItem.toStream
    } yield (quizItem, QuizGroupWithHeader(header, quizGroup))).headOption.orElse(
        findAnyUnfinishedQuizItem)

  /*
   * Near the end of the quiz, there will be items that are "nearly learnt" because they
   * have been answered correctly several times, but are not considered presentable under
   * normal criteria, because the last correct response was recent. However, they do need
   * to be presented in order for the quiz to finish, so this method is called as a last try.
   */
  def findAnyUnfinishedQuizItem: Option[(QuizItemViewWithChoices, QuizGroupWithHeader)] =
    (for {
      (header, quizGroup) <- activeQuizGroups
      quizItem <- QuizGroupWithHeader(header, quizGroup).findAnyUnfinishedQuizItem.toStream
    } yield (quizItem, QuizGroupWithHeader(header, quizGroup))).headOption

  def resultsBeginningWith(input: String): List[SearchResult] =
    activeQuizGroups.flatMap {
      case (header, quizGroup) => Dictionary.convertToSearchResults(
        quizGroup.dictionary.mappingsForKeysBeginningWith(input), header)
    }.toList

  def resultsContaining(input: String): List[SearchResult] =
    activeQuizGroups.flatMap {
      case (header, quizGroup) => Dictionary.convertToSearchResults(
        quizGroup.dictionary.mappingsForKeysContaining(input), header)
    }.toList

  def isCorrect(quizGroupHeader: QuizGroupHeader, prompt: String, userResponse: String):
      Boolean = {
    val responses = findResponsesFor(prompt, quizGroupHeader)
    responses.exists(TextValue(_).looselyMatches(userResponse))
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

  def updatedPromptNumber(qgWithHeader: QuizGroupWithHeader): Quiz =
    setQuizGroup(qgWithHeader.header,
        QuizGroup.promptNumberLens.mod( (1+), qgWithHeader.quizGroup))

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

  def addQuizItemToFront(header: QuizGroupHeader, prompt: String, response: String): Quiz =
    Quiz.quizGroupsLens.set(this,
      mapVPLens(header) mod ((_: QuizGroup).addNewQuizItem(prompt, response), quizGroups)
    )

  def removeQuizItem(prompt: String, response: String, header: QuizGroupHeader): (Quiz, Boolean) =
    removeQuizItem(QuizItem(prompt, response), header)

  def existsQuizItem(quizItem: QuizItem, header: QuizGroupHeader) =
    mapVPLens(header).get(activeQuizGroups).map(_.contains(quizItem)).isDefined

  def removeQuizItem(quizItem: QuizItem, header: QuizGroupHeader): (Quiz, Boolean) = {
    val quizItemExisted = existsQuizItem(quizItem, header)
    val quiz: Quiz = Quiz.quizGroupsLens.set(this,
        mapVPLens(header) mod ((_: QuizGroup).removeQuizItem(quizItem), quizGroups))
    (quiz, quizItemExisted)
  }

  /*
   * This is intended for reversible quiz items, principally word translations.
   */
  def addQuizItemToFrontOfTwoGroups(header: QuizGroupHeader,
      prompt: String, response: String): Quiz = {

    // E.g. add to the English -> German group
    val quizUpdated1 = addQuizItemToFront(header, prompt, response)

    // E.g. add to the German -> English group
    val quizUpdated2 = quizUpdated1.addQuizItemToFront(header.reverse, response, prompt)

    quizUpdated2
  }

  def qgCurrentPromptNumber(header: QuizGroupHeader): Option[Int] =
    mapVPLens(header).get(activeQuizGroups).map(_.currentPromptNumber)

  def findQuizItem(header: QuizGroupHeader, prompt: String, response: String):
      Option[QuizItem] =
    mapVPLens(header).get(activeQuizGroups).flatMap(_.findQuizItem(prompt, response))

  def updateWithUserResponse(isCorrect: Boolean, quizGroupHeader: QuizGroupHeader,
      quizItem: QuizItem): Quiz = {
    qgCurrentPromptNumber(quizGroupHeader) match {
      case Some(qgPromptNumber) =>
        val userAnswer = new UserResponse(qgPromptNumber)

        Quiz.quizGroupsLens.set(this,
            mapVPLens(quizGroupHeader) mod ((_: QuizGroup).updatedWithUserResponse(
                quizItem.prompt, quizItem.correctResponse, isCorrect,
                quizItem.userResponses, userAnswer), quizGroups)
        )
      case _ => this
    }
  }

  def nearTheEnd = quizGroups.exists(qgwh =>
      (qgwh._2.numPrompts - qgwh._2.currentPromptNumber) < Criteria.maxDiffInPromptNumMinimum)
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

  def fromCustomFormat(str: String): Quiz = {
    val quizGroupHeadings = str.split("quizGroup").tail
    val quizGroups = quizGroupHeadings.map(
        headerText => QuizGroupHeader(headerText) -> QuizGroup()).toMap
    Quiz(quizGroups)
  }

  def demoQuiz(quizGroupsData: List[String] = demoDataInCustomFormat): Quiz = {
    l.log("Using demo data")
    val qgsWithHeader: Iterable[QuizGroupWithHeader] =
        quizGroupsData.map(QuizGroupWithHeader.fromCustomFormat(_) )
    Quiz(qgsWithHeader.map(
        qgWithHeader => Pair(qgWithHeader.header, qgWithHeader.quizGroup)).toMap)
  }

  // Demo data to use as a fallback if no file is available
  def demoDataInCustomFormat = List(

    "quizGroup type=\"WordMapping\" promptType=\"English word\" responseType=\"German word\" currentPromptNumber=\"0\" isActive=\"true\"\n" +
    "en route|unterwegs\n" +
    "contract|Vertrag\n" +
    "treaty|Vertrag\n" +
    "against|wider\n" +
    "entertain|unterhalten\n",

    "quizGroup type=\"WordMapping\" promptType=\"German word\" responseType=\"English word\" currentPromptNumber=\"0\" isActive=\"true\"\n" +
    "unterwegs|en route\n" +
    "Vertrag|contract\n" +
    "Vertrag|treaty\n" +
    "wider|against\n" +
    "unterhalten|entertain"
  )

}