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
import com.oranda.libanius.model.wordmapping.WordMappingValueSet

case class Quiz(quizGroups: Set[QuizGroup] = ListSet()) extends ModelComponent {

  val l = AppDependencies.logger

  def copy(newPromptNumber: Int) = Quiz(quizGroups)

  /*
   * This serialization does not include QuizGroup data, only metadata.
   *
   * Example:
   * quiz
   *   quizGroup type="WordMapping" keyType="German word" valueType="English word"
   *   quizGroup type="WordMapping" keyType="English word" valueType="German word"
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

  def findValuesFor(keyWord: String, header: QuizGroupHeader): List[String] = {
    l.log("Looking for values for " + keyWord + " in " + header)

    findQuizGroup(header) match {
      case Some(quizGroup) =>
        quizGroup.findValuesFor(keyWord).map(_.value)
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

  // This will replace any existing wordMappingGroup with the same key-value pair
  def addQuizGroup(quizGroup: QuizGroup): Quiz = {
    val newQuiz: Quiz = removeQuizGroup(quizGroup.header)
    Quiz(newQuiz.quizGroups + quizGroup)
  }


  def removeKey(keyWord: String, header: QuizGroupHeader): Quiz = {
    val quizGroup = findQuizGroup(header)
    quizGroup match {
      case Some(quizGroup) => replaceQuizGroup(quizGroup.removeQuizPairsForKey(keyWord))
      case None => this
    }
  }

  def removeQuizPair(keyWord: String, quizValue: QuizValueWithUserAnswers,
      header: QuizGroupHeader): Quiz = {
    val wordMappingGroup = findQuizGroup(header)
    wordMappingGroup match {
      case Some(quizGroup) =>
        val quizPair = QuizPair(keyWord, quizValue)
        val newQuizGroup = quizGroup.removeQuizPair(quizPair)
        replaceQuizGroup(newQuizGroup)
      case None => this
    }
  }

  /*
   * Find the first available "presentable" word mapping: imperative version.
   * Return a quiz item, the quiz group it belongs to, and a list of quiz groups which failed
   * to return anything.
   */
  def findQuizItem:
      Pair[Option[(QuizItemViewWithChoices, QuizGroup)], List[QuizGroup]] = {

    var failedWmgs = List[QuizGroup]()
    var quizItemPair: Option[(QuizItemViewWithChoices, QuizGroup)] = None

    val quizGroupIter = quizGroups.iterator
    while (quizGroupIter.hasNext && !quizItemPair.isDefined) {
      val quizGroup = quizGroupIter.next
      quizGroup.findPresentableQuizItem match {
        case Some(quizItem) => quizItemPair = Some(Pair(quizItem, quizGroup))
        case _ => failedWmgs ::= quizGroup
      }
    }
    quizItemPair = quizItemPair.orElse(findAnyUnfinishedQuizItem)
    (quizItemPair, failedWmgs)
  }

  def findAnyUnfinishedQuizItem: Option[(QuizItemViewWithChoices, QuizGroup)] =
    (for {
      quizGroup <- quizGroups.toStream
      quizItem <- quizGroup.findAnyUnfinishedQuizItem.toStream
    } yield (quizItem, quizGroup)).headOption

  def addWordMappingToFront(header: QuizGroupHeader, keyWord: String, value: String): Quiz = {
    val quizGroup = findQuizGroup(header)
    quizGroup.map(quizGroup => replaceQuizGroup(quizGroup.addQuizPairToFront(
        QuizPair(keyWord, QuizValueWithUserAnswers(value))))).getOrElse(this)
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
    val userAnswer = new UserAnswer(isCorrect, currentQuizItem.qgCurrentPromptNumber)
    val quizGroup: Option[QuizGroup] = findQuizGroup(currentQuizItem.quizGroupHeader)
    quizGroup match {
      case Some(quizGroup) =>
        val quizGroupUpdated = quizGroup.updatedWithUserAnswer(currentQuizItem.keyWord,
            currentQuizItem.quizValue, userAnswer)
        addQuizGroup(quizGroupUpdated)
      case _ => this
    }
  }

  def numGroups = quizGroups.size
  def numKeyWords = quizGroups.map(_.numKeyWords).sum
  def numValues = quizGroups.map(_.numValues).sum

  def scoreSoFar: BigDecimal =   // out of 1.0
    numCorrectAnswers.toDouble / (size * AppDependencies.conf.numCorrectAnswersRequired).toDouble

  def size = quizGroups.map(_.size).sum
  def numCorrectAnswers = quizGroups.map(_.numCorrectAnswers).sum

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
    var quiz = Quiz()
    val quizGroupHeadings = str.split("quizGroup").tail

    quizGroupHeadings.foreach {
      quizGroupHeading => val quizGroup = QuizGroup(QuizGroupHeader(quizGroupHeading))
                    quiz = quiz.addQuizGroup(quizGroup)
    }
    quiz
  }

  def demoQuiz(quizGroupsData: List[String] = demoDataInCustomFormat): Quiz = {
    l.log("Using demo data")
    val quizGroups = quizGroupsData.map(QuizGroup.fromCustomFormat(_))
    quizGroups.foldLeft(Quiz())((acc, quizGroup) => acc.addQuizGroup(quizGroup))
    // TODO: watch out when we're saving, we're not overwriting anything
  }

  // Demo data to use as a fallback if no file is available
  def demoDataInCustomFormat = List(

    "quizGroup type=\"WordMapping\" keyType=\"English word\" valueType=\"German word\" currentPromptNumber=\"0\"\n" +
    "en route|unterwegs\n" +
    "contract|Vertrag\n" +
    "treaty|Vertrag\n" +
    "against|wider\n" +
    "entertain|unterhalten\n",

    "quizGroup type=\"WordMapping\" keyType=\"German word\" valueType=\"English word\" currentPromptNumber=\"0\"\n" +
    "unterwegs|en route\n" +
    "Vertrag|contract/treaty\n" +
    "wider|against\n" +
    "unterhalten|entertain"
  )

}