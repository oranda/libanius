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

import scala.math.BigDecimal.double2bigDecimal
import scala.collection.immutable.List
import com.oranda.libanius.dependencies.AppDependencies
import com.oranda.libanius.model.quizitem.{QuizItemViewWithChoices, QuizItem}

case class Quiz(quizGroups: Set[QuizGroup] = ListSet()) extends ModelComponent {

  val l = AppDependencies.logger

  def copy(newPromptNumber: Int) = Quiz(quizGroups)

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

    def quizGroupMetadata(strBuilder: StringBuilder, quizGroup: QuizGroup) =
      quizGroup.header.toCustomFormat(strBuilder)

    StringUtil.mkString(strBuilder, quizGroups, quizGroupMetadata, '\n')
  }

  def findOrAddQuizGroup(header: QuizGroupHeader):
      (Quiz, QuizGroup) = {
    val wordMappingGroup = findQuizGroup(header)
    wordMappingGroup match {
      case Some(wordMappingGroup) => (this, wordMappingGroup)
      case None => val wordMappingGroup: QuizGroup = QuizGroup(header)
                   (addQuizGroup(wordMappingGroup), wordMappingGroup)
    }
  }

  /*
   * Do not call in a loop: not fast.
   */
  def findResponsesFor(prompt: String, header: QuizGroupHeader): List[String] = {
    l.log("Looking for values for " + prompt + " in " + header)

    findQuizGroup(header) match {
      case Some(quizGroup) => quizGroup.findResponsesFor(prompt)
      case _ => l.logError("could not find quizGroup for " + header)
        Nil
    }
  }

  /*
   *  Do not call in a loop: not fast.
   */
  def findPromptsFor(response: String, header: QuizGroupHeader): List[String] = {
    l.log("Looking for prompt for " + response + " in " + header)

    findQuizGroup(header) match {
      case Some(quizGroup) => quizGroup.findPromptsFor(response)
      case _ => l.logError("could not find quizGroup for " + header)
        Nil
    }
  }

  def findQuizGroup(header: QuizGroupHeader): Option[QuizGroup] =
    Quiz.findQuizGroup(quizGroups, header)

  def removeQuizGroup(header: QuizGroupHeader): Quiz = {
    val wordMappingGroupsFiltered = quizGroups.filterNot(_.header.matches(header))
    Quiz(wordMappingGroupsFiltered)
  }

  // Just a synonym for addQuizGroup
  def replaceQuizGroup(quizGroup: QuizGroup) = addQuizGroup(quizGroup)

  // This will replace any existing wordMappingGroup with the same prompt-response pair
  def addQuizGroup(quizGroup: QuizGroup): Quiz = {
    val newQuiz: Quiz = removeQuizGroup(quizGroup.header)
    Quiz(newQuiz.quizGroups + quizGroup)
  }

  def removeQuizItemsForPrompt(prompt: String, header: QuizGroupHeader): Quiz =
    (for (quizGroup <- findQuizGroup(header))
      yield quizGroup.removeQuizItemsForPrompt(prompt)).map(replaceQuizGroup(_)).getOrElse(this)

  def removeQuizItem(prompt: String, response: String, header: QuizGroupHeader): (Quiz, Boolean) =
    removeQuizItem(QuizItem(prompt, response), header)

  def removeQuizItem(quizItem: QuizItem, header: QuizGroupHeader): (Quiz, Boolean) = {
    val updatedQuiz: Option[Quiz] = (for (
      quizGroup <- findQuizGroup(header)
      if quizGroup.contains(quizItem)
    ) yield quizGroup.removeQuizItem(quizItem)).map(replaceQuizGroup(_))

    val wasRemoved = updatedQuiz.isDefined
    (updatedQuiz.getOrElse(this), wasRemoved)
  }

  /*
   * Find the first available "presentable" quiz item.
   * Return a quiz item and the quiz group it belongs to.
   */
  def findPresentableQuizItem: Option[(QuizItemViewWithChoices, QuizGroup)] = {
    val quizItem = (for {
      quizGroup <- quizGroups.toStream
      quizItem <- quizGroup.findPresentableQuizItem.toStream
    } yield (quizItem, quizGroup)).headOption
    quizItem.orElse(findAnyUnfinishedQuizItem)
  }

  def findAnyUnfinishedQuizItem: Option[(QuizItemViewWithChoices, QuizGroup)] =
    (for {
      quizGroup <- quizGroups.toStream
      quizItem <- quizGroup.findAnyUnfinishedQuizItem.toStream
    } yield (quizItem, quizGroup)).headOption

  def addWordMappingToFront(header: QuizGroupHeader, prompt: String, value: String): Quiz = {
    val quizGroup = findQuizGroup(header)
    quizGroup.map(quizGroup => replaceQuizGroup(quizGroup.addQuizItemToFront(
        QuizItem(prompt, value)))).getOrElse(this)
  }

  def addWordMappingToFrontOfTwoGroups(header: QuizGroupHeader,
      keyWord: String, value: String): Quiz = {

    l.log("Adding to 2 quizGroups: " + keyWord + "," + value)
    // E.g. add to the English -> German group
    val quizUpdated1 = addWordMappingToFront(header, keyWord, value)

    // E.g. add to the German -> English group
    val quizUpdated2 = quizUpdated1.addWordMappingToFront(header.reverse, value, keyWord)

    quizUpdated2
  }

  def updateWithUserAnswer(isCorrect: Boolean, currentQuizItem: QuizItemViewWithChoices): Quiz = {
    val userAnswer = new UserResponse(currentQuizItem.qgCurrentPromptNumber)
    val quizGroup: Option[QuizGroup] = findQuizGroup(currentQuizItem.quizGroupHeader)
    quizGroup match {
      case Some(quizGroup) =>
        val quizGroupUpdated = quizGroup.updatedWithUserAnswer(currentQuizItem.prompt,
            currentQuizItem.response, isCorrect, currentQuizItem.quizItem.userResponses, userAnswer)
        addQuizGroup(quizGroupUpdated)
      case _ => this
    }
  }

  def numGroups = quizGroups.size
  def numPrompts = (0 /: quizGroups) { case (sum, qg) => sum + qg.numPrompts }
  def numResponses = (0 /: quizGroups) { case (sum, qg) => sum + qg.numResponses }

  def scoreSoFar: BigDecimal = // out of 1.0
    numCorrectAnswers.toDouble / (size * AppDependencies.conf.numCorrectAnswersRequired).toDouble

  def size = (0 /: quizGroups) { case (sum, qg) => sum + qg.size }
  def numCorrectAnswers = (0 /: quizGroups) { case (sum, qg) => sum + qg.numCorrectAnswers }

  def merge(otherQuiz: Quiz): Quiz = {
    val wordMappingGroupsCombined = otherQuiz.quizGroups.foldLeft(quizGroups) {
      (acc, otherWmg) =>
        val quizGroup = findQuizGroup(otherWmg.header)
        val quizGroupMerged = otherWmg.merge(quizGroup)
        quizGroups.filterNot(Some(_) == quizGroup) + quizGroupMerged
    }
    Quiz(wordMappingGroupsCombined)
  }
}

object Quiz {

  val l = AppDependencies.logger

  def findQuizGroup(quizGroups: Set[QuizGroup], header: QuizGroupHeader):
      Option[QuizGroup] =
    quizGroups.find(_.header.matches(header))

  def metadataFromCustomFormat(str: String): Set[QuizGroupHeader] = {
    val quizGroupHeadings = str.split("quizGroup").tail
    quizGroupHeadings.map(QuizGroupHeader(_)).toSet
  }

  def fromCustomFormat(str: String): Quiz = {
    val quizGroupHeadings = str.split("quizGroup").tail
    val quizGroups = quizGroupHeadings.map(headerText => QuizGroup(QuizGroupHeader(headerText)))
    Quiz(quizGroups.toSet)
  }

  def demoQuiz(quizGroupsData: List[String] = demoDataInCustomFormat): Quiz = {
    l.log("Using demo data")
    val quizGroups = quizGroupsData.map(QuizGroup.fromCustomFormat(_))
    Quiz(quizGroups.toSet)
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