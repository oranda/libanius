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

import com.oranda.libanius.util.StringUtil

import scala.language.postfixOps
import scala.math.BigDecimal.double2bigDecimal
import com.oranda.libanius.dependencies._
import com.oranda.libanius.model.quizitem.QuizItem
import com.oranda.libanius.model.wordmapping.Dictionary

import java.lang.StringBuilder

import scalaz._
import scalaz.std.set
import Scalaz._, PLens._
import com.oranda.libanius.model.quizitem.QuizItemViewWithChoices

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
  def numItems = (0 /: activeQuizGroups.values)(_ + _.size)
  def numCorrectAnswers = (0 /: activeQuizGroups.values)(_ + _.numCorrectAnswers)
  def scoreSoFar: BigDecimal =  // out of 1.0
    numCorrectAnswers.toDouble / (numItems * conf.numCorrectAnswersRequired).toDouble

  /*
   * Find the first available "presentable" quiz item.
   * Return a quiz item and the quiz group it belongs to.
   */
  def findPresentableQuizItem: Option[(QuizItemViewWithChoices, QuizGroupWithHeader)] = {
    val quizItem = (for {
      (header, quizGroup) <- activeQuizGroups
      quizItem <- quizGroup.findPresentableQuizItem(header).toStream
    } yield (quizItem, QuizGroupWithHeader(header, quizGroup))).headOption
    quizItem.orElse(findAnyUnfinishedQuizItem)
  }

  def findAnyUnfinishedQuizItem: Option[(QuizItemViewWithChoices, QuizGroupWithHeader)] =
    (for {
      (header, quizGroup) <- activeQuizGroups
      quizItem <- quizGroup.findAnyUnfinishedQuizItem(header).toStream
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
    addQuizItemToFront(header, QuizItem(prompt, response))

  def addQuizItemToFront(header: QuizGroupHeader, quizItem: QuizItem): Quiz = {
    Quiz.quizGroupsLens.set(this,
      mapVPLens(header) mod ((_: QuizGroup).addQuizItemToFront(quizItem), quizGroups)
    )
  }

  def removeQuizItemsForPrompt(prompt: String, header: QuizGroupHeader): Quiz =
    Quiz.quizGroupsLens.set(this,
      mapVPLens(header) mod ((_: QuizGroup).removeQuizItemsForPrompt(prompt), quizGroups)
    )

  def removeQuizItem(prompt: String, response: String, header: QuizGroupHeader): (Quiz, Boolean) =
    removeQuizItem(QuizItem(prompt, response), header)

  def existsQuizItem(quizItem: QuizItem, header: QuizGroupHeader) =
    mapVPLens(header).get(activeQuizGroups).map(_.contains(quizItem)).isDefined

  def removeQuizItem(quizItem: QuizItem, header: QuizGroupHeader): (Quiz, Boolean) = {
    val quizItemExisted = existsQuizItem(quizItem, header)
    val quizItemsLens = ~QuizGroup.quizGroupItemsLens compose mapVPLens(header)
    val quiz: Quiz = Quiz.quizGroupsLens.set(this,
        quizItemsLens.mod(QuizGroup.remove(_: Stream[QuizItem], quizItem), quizGroups))
    (quiz, quizItemExisted)
  }

  /*
   * This is intended for reversible quiz items, principally word translations.
   */
  def addQuizItemToFrontOfTwoGroups(header: QuizGroupHeader,
      prompt: String, response: String): Quiz = {

    l.log("Adding to 2 quizGroups: " + prompt + "," + response)
    // E.g. add to the English -> German group
    val quizUpdated1 = addQuizItemToFront(header, prompt, response)

    // E.g. add to the German -> English group
    val quizUpdated2 = quizUpdated1.addQuizItemToFront(header.reverse, response, prompt)

    quizUpdated2
  }

  def updateWithUserAnswer(isCorrect: Boolean, currentQuizItem: QuizItemViewWithChoices): Quiz = {
    val userAnswer = new UserResponse(currentQuizItem.qgCurrentPromptNumber)
    Quiz.quizGroupsLens.set(this,
        mapVPLens(currentQuizItem.quizGroupHeader) mod ((_: QuizGroup).updatedWithUserAnswer(
            currentQuizItem.prompt, currentQuizItem.response, isCorrect,
            currentQuizItem.quizItem.userResponses, userAnswer), quizGroups)
    )
  }
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
    val quizGroups: Iterable[QuizGroupWithHeader] =
      quizGroupsData.map(QuizGroup.fromCustomFormat(_))
    Quiz(quizGroups.map(qgWithHeader => Pair(qgWithHeader.header, qgWithHeader.quizGroup)).toMap)
  }

  // Demo data to use as a fallback if no file is available
  def demoDataInCustomFormat = List(

    "quizGroup type=\"WordMapping\" promptType=\"English word\" responseType=\"German word\" currentPromptNumber=\"0\"\n" +
    "en route|unterwegs\n" +
    "contract|Vertrag\n" +
    "treaty|Vertrag\n" +
    "against|wider\n" +
    "entertain|unterhalten\n",

    "quizGroup type=\"WordMapping\" promptType=\"German word\" responseType=\"English word\" currentPromptNumber=\"0\"\n" +
    "unterwegs|en route\n" +
    "Vertrag|contract/treaty\n" +
    "wider|against\n" +
    "unterhalten|entertain"
  )

}
