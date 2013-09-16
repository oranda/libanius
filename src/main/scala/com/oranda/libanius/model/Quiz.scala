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
import com.oranda.libanius.model.quizitem.{QuizItem}
import com.oranda.libanius.model.wordmapping.Dictionary
import com.oranda.libanius.model.quizitem.QuizItemViewWithChoices

import java.lang.StringBuilder

import scalaz._
import scalaz.PLens._
import scalaz.std.set


case class Quiz(quizGroups: Map[QuizGroupHeader, QuizGroup] = ListMap()) extends ModelComponent {

  /*
   * This serialization does not include QuizGroup data, only metadata.
   *
   * Example:
   * quiz
   *   quizGroup type="WordMapping" promptType="German word" responseType="English word"
   *   quizGroup type="WordMapping" promptType="English word" responseType="German word"
   */
  def toCustomFormat: StringBuilder = {
    // For efficiency, avoiding Scala's own StringBuilder and mkString
    val strBuilder = new StringBuilder("quiz\n")

    def quizGroupMetadata(strBuilder: StringBuilder, quizGroupHeader: QuizGroupHeader) =
      quizGroupHeader.toCustomFormat(strBuilder)

    StringUtil.mkString(strBuilder, quizGroups.keySet, quizGroupMetadata, '\n')
  }

  /*
   * Do not call in a loop: not fast.
   */
  def findResponsesFor(prompt: String, header: QuizGroupHeader): List[String] =
    findQuizGroup(header) match {
      case Some(quizGroup) => quizGroup.findResponsesFor(prompt)
      case _ => l.logError("could not find quizGroup for " + header)
                Nil
    }

  /*
   *  Do not call in a loop: not fast.
   */
  def findPromptsFor(response: String, header: QuizGroupHeader): List[String] = {

    findQuizGroup(header) match {
      case Some(quizGroup) => quizGroup.findPromptsFor(response)
      case _ => l.logError("could not find quizGroup for " + header)
                Nil
    }
  }

  def findQuizGroup(header: QuizGroupHeader): Option[QuizGroup] = quizGroups.get(header)

  def updatedPromptNumber(qgWithHeader: QuizGroupWithHeader): Quiz =
    //replaceQuizGroup(qgWithHeader.header, qgWithHeader.quizGroup.updatedPromptNumber)
    replaceQuizGroup(qgWithHeader.header, QuizGroup.promptNumberLens.mod( (1+), qgWithHeader.quizGroup))


  def removeQuizGroup(header: QuizGroupHeader): Quiz =
    updatedQuizGroups(quizGroups - header)

  def updatedQuizGroups(newQuizGroups: Map[QuizGroupHeader, QuizGroup]): Quiz =
    Quiz.quizGroupsLens.set(this, newQuizGroups)

  // Just a synonym for replaceQuizGroup
  def addQuizGroup(header: QuizGroupHeader, quizGroup: QuizGroup): Quiz =
    replaceQuizGroup(header, quizGroup)

  // This will replace any existing wordMappingGroup with the same prompt-response pair
  def replaceQuizGroup(header: QuizGroupHeader, quizGroup: QuizGroup) =
    Quiz.quizGroupsLens.set(this, quizGroups + (header -> quizGroup))

  /*
   * Find the first available "presentable" quiz item.
   * Return a quiz item and the quiz group it belongs to.
   */
  def findPresentableQuizItem: Option[(QuizItemViewWithChoices, QuizGroupWithHeader)] = {
    val quizItem = (for {
      (header, quizGroup) <- quizGroups
      quizItem <- quizGroup.findPresentableQuizItem(header).toStream
    } yield (quizItem, QuizGroupWithHeader(header, quizGroup))).headOption
    quizItem.orElse(findAnyUnfinishedQuizItem)
  }

  def findAnyUnfinishedQuizItem: Option[(QuizItemViewWithChoices, QuizGroupWithHeader)] =
    (for {
      (header, quizGroup) <- quizGroups
      quizItem <- quizGroup.findAnyUnfinishedQuizItem(header).toStream
    } yield (quizItem, QuizGroupWithHeader(header, quizGroup))).headOption

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

  def existsQuizItem(quizItem: QuizItem, header: QuizGroupHeader) = {
    val quizGroup = Quiz.quizGroupMapLens(header).get(quizGroups)
    quizGroup.map(_.contains(quizItem)).isDefined
  }

  def removeQuizItem(quizItem: QuizItem, header: QuizGroupHeader): (Quiz, Boolean) = {
    val quizItemExisted = existsQuizItem(quizItem, header)
    val quiz = Quiz.quizGroupsLens.set(this,
      mapVPLens(header) mod ((_: QuizGroup).removeQuizItem(quizItem), quizGroups)
    )
    (quiz, quizItemExisted)
  }

  // TODO: replace this with proper lens composition or abandon
  private def setQuizItems(qgHeader: QuizGroupHeader, quizGroup: QuizGroup,
      quizItems: Stream[QuizItem]): Quiz = {
    val newQuizGroup = QuizGroup.quizGroupItemsLens.set(quizGroup, quizItems)
    Quiz.quizGroupMapLens(qgHeader).set(quizGroups, Some(newQuizGroup))
    Quiz.quizGroupsLens.set(this, quizGroups + (qgHeader -> newQuizGroup))
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

  def numGroups = quizGroups.size
  def numPrompts = (0 /: quizGroups.values)(_ + _.numPrompts)
  def numResponses = (0 /: quizGroups.values)(_ + _.numResponses)

  def scoreSoFar: BigDecimal =  // out of 1.0
    numCorrectAnswers.toDouble / (numItems * conf.numCorrectAnswersRequired).toDouble

  def numItems = (0 /: quizGroups.values)(_ + _.size)
  def numCorrectAnswers = (0 /: quizGroups.values)(_ + _.numCorrectAnswers)

  def resultsBeginningWith(input: String): List[SearchResult] =
    quizGroups.flatMap {
      case (header, quizGroup) => Dictionary.convertToSearchResults(
          quizGroup.dictionary.mappingsForKeysBeginningWith(input), header)
    }.toList

  def resultsContaining(input: String): List[SearchResult] =
    quizGroups.flatMap {
      case (header, quizGroup) => Dictionary.convertToSearchResults(
          quizGroup.dictionary.mappingsForKeysContaining(input), header)
    }.toList
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
    // TODO: watch out when we're saving, we're not overwriting anything
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