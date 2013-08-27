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

package com.oranda.libanius.model.wordmapping

import scala.collection.immutable._

import com.oranda.libanius.model._
import com.oranda.libanius.util.StringUtil

import scala.math.BigDecimal.double2bigDecimal
import scala.collection.immutable.List
import com.oranda.libanius.dependencies.AppDependencies
import com.oranda.libanius.model

case class QuizOfWordMappings(wordMappingGroups: Set[WordMappingGroup] = ListSet())
    extends Quiz {

  val l = AppDependencies.logger

  def copy(newPromptNumber: Int) = new QuizOfWordMappings(wordMappingGroups)

  /*
   * This serialization does not include wordMappingGroup (wmg) data, only metadata.
   *
   * Example:
   * quizOfWordMappings
   * quizGroup type="WordMapping" keyType="German word" valueType="English word"
   * quizGroup type="WordMapping" keyType="English word" valueType="German word"
   */
  def toCustomFormat: StringBuilder = {
    // For efficiency, avoiding Scala's own StringBuilder and mkString
    val strBuilder = new StringBuilder("quizOfWordMappings\n")

    def wmgMetadata(strBuilder: StringBuilder, wmg: WordMappingGroup) =
      strBuilder.append("quizGroup type=\"WordMapping\" keyType=\"").append(wmg.keyType).
          append("\" valueType=\"").append(wmg.valueType).append("\"")

    StringUtil.mkString(strBuilder, wordMappingGroups, wmgMetadata, '\n')
  }

  def findOrAddWordMappingGroup(header: model.QuizGroupHeader):
      (QuizOfWordMappings, WordMappingGroup) = {
    val wordMappingGroup = findWordMappingGroup(header)
    wordMappingGroup match {
      case Some(wordMappingGroup) => (this, wordMappingGroup)
      case None => val wordMappingGroup: WordMappingGroup = WordMappingGroup(header)
                   (addWordMappingGroup(wordMappingGroup), wordMappingGroup)
    }
  }

  def findValuesFor(keyWord: String, header: model.QuizGroupHeader): List[String] = {
    l.log("Looking for values for " + keyWord + " in " + header)

    findWordMappingGroup(header) match {
      case Some(quizGroup) => quizGroup.findValueSetFor(keyWord) match {
        case Some(wordMappingValueSet) => wordMappingValueSet.strings.toList
        case _ => l.log("quizGroup numKeyWords = " + quizGroup.numKeyWords)
          l.log("quizGroup first 10 wordMappings: " + quizGroup.quizPairs.take(10))
          l.logError("could not find valueSet for keyWord " + keyWord)
          Nil
      }
      case _ => l.logError("could not find quizGroup for " + header)
          Nil
    }
  }

  def findWordMappingGroup(header: model.QuizGroupHeader): Option[WordMappingGroup] =
    QuizOfWordMappings.findWordMappingGroup(wordMappingGroups, header)

  def removeWordMappingGroup(header: model.QuizGroupHeader): QuizOfWordMappings = {
    val wordMappingGroupsFiltered = wordMappingGroups.filterNot(_.header.matches(header))
    new QuizOfWordMappings(wordMappingGroupsFiltered)
  }

  // Just a synonym for addWordMappingGroup
  def replaceWordMappingGroup(wmg: WordMappingGroup) = addWordMappingGroup(wmg)

  // This will replace any existing wordMappingGroup with the same key-value pair
  def addWordMappingGroup(wmg: WordMappingGroup): QuizOfWordMappings = {
    val newQuiz = removeWordMappingGroup(wmg.header)
    new QuizOfWordMappings(newQuiz.wordMappingGroups + wmg)
  }

  def removeWord(keyWord: String, header: model.QuizGroupHeader): QuizOfWordMappings = {
    val wordMappingGroup = findWordMappingGroup(header)
    wordMappingGroup match {
      case Some(wmg) => replaceWordMappingGroup(wmg.removeWordMapping(keyWord))
      case None => this
    }
  }

  def removeWordMappingValue(keyWord: String, wordMappingValue: QuizValueWithUserAnswers,
      header: QuizGroupHeader): (QuizOfWordMappings, Boolean) = {
    val wordMappingGroup = findWordMappingGroup(header)
    wordMappingGroup match {
      case Some(wmg) =>
        val (newWmg, wasRemoved) = wmg.removeWordMappingValue(keyWord, wordMappingValue)
        (replaceWordMappingGroup(newWmg), wasRemoved)
      case None => (this, false)
    }
  }

  /*
   * Find the first available "presentable" word mapping: imperative version.
   * Return a quiz item, the quiz group it belongs to, and a list of quiz groups which failed
   * to return anything.
   */
  def findQuizItem:
      Pair[Option[(QuizItemViewWithChoices[_], WordMappingGroup)], List[WordMappingGroup]] = {

    var failedWmgs = List[WordMappingGroup]()
    var quizItemPair: Option[(QuizItemViewWithChoices[_], WordMappingGroup)] = None

    val wmgIter = wordMappingGroups.iterator
    while (wmgIter.hasNext && !quizItemPair.isDefined) {
      val wmg = wmgIter.next
      wmg.findPresentableQuizItem match {
        case Some(quizItem) => quizItemPair = Some(Pair(quizItem, wmg))
        case _ => failedWmgs ::= wmg
      }
    }
    quizItemPair = quizItemPair.orElse(findAnyUnfinishedQuizItem)
    (quizItemPair, failedWmgs)
  }

  def findAnyUnfinishedQuizItem: Option[(QuizItemViewWithChoices[_], WordMappingGroup)] =
    (for {
      wmg <- wordMappingGroups.toStream
      quizItem <- wmg.findAnyUnfinishedQuizItem.toStream
    } yield (quizItem, wmg)).headOption

  def addWordMappingToFront(header: model.QuizGroupHeader, keyWord: String, value: String):
      QuizOfWordMappings = {
    val wmg = findWordMappingGroup(header)
    wmg.map(wmg =>
        replaceWordMappingGroup(wmg.addWordMappingToFront(keyWord, value).resetSearchRange)).
        getOrElse(this)
  }

  def addWordMappingToFrontOfTwoGroups(header: model.QuizGroupHeader,
      keyWord: String, value: String): QuizOfWordMappings = {

    l.log("Adding to 2 wmgs: " + keyWord + "," + value)
    // E.g. add to the English -> German group
    val quizUpdated1 = addWordMappingToFront(header, keyWord, value)

    // E.g. add to the German -> English group
    val quizUpdated2 = quizUpdated1.addWordMappingToFront(header.reverse, value, keyWord)

    quizUpdated2
  }

  def updateWithUserAnswer(isCorrect: Boolean, currentQuizItem: QuizItemViewWithChoices[_]):
      QuizOfWordMappings = {
    val userAnswer = new UserAnswer(isCorrect, currentQuizItem.qgCurrentPromptNumber)
    val wmg: Option[WordMappingGroup] = findWordMappingGroup(currentQuizItem.quizGroupHeader)
    wmg match {
      case Some(wmg) =>
        val wmgUpdated = wmg.updateWithUserAnswer(currentQuizItem.keyWord,
            currentQuizItem.wmvs.asInstanceOf[WordMappingValueSet],
            currentQuizItem.quizValue, userAnswer)
        addWordMappingGroup(wmgUpdated)
      case _ => this
    }
  }

  /*
   * Return a quiz with wmg's updated to have changed "search ranges" for wmg's where a
   * search just failed. The point of this is to improve performance for the next search.
   */
  def updateRangeForFailedWmgs(wmgsFailed: List[WordMappingGroup]): QuizOfWordMappings = {
    l.log("updating search ranges for wmgs: " + wmgsFailed.map(_.header).mkString(";"))
    val wmgsWithUpdatedRange = wmgsFailed.map(_.updatedSearchRange)
    wmgsWithUpdatedRange.foldLeft(this)((acc, wmg) => acc.addWordMappingGroup(wmg))
  }

  def numGroups = wordMappingGroups.size

  def numKeyWords = wordMappingGroups.foldLeft(0)(_ + _.numKeyWords)

  def numItems: Int = wordMappingGroups.foldLeft(0)(_ + _.numValues)

  override def scoreSoFar: BigDecimal = {  // out of 1.0
    val _numItemsAndCorrectAnswers = numItemsAndCorrectAnswers
    val scoreSoFar = _numItemsAndCorrectAnswers._2.toDouble /
        (_numItemsAndCorrectAnswers._1 * AppDependencies.conf.numCorrectAnswersRequired).toDouble
    scoreSoFar
  }

  def numCorrectAnswers = numItemsAndCorrectAnswers._2

  def numItemsAndCorrectAnswers =
    wordMappingGroups.foldLeft(Pair(0, 0))((acc, group) =>
        (acc._1 + group.numItemsAndCorrectAnswers._1,
         acc._2 + group.numItemsAndCorrectAnswers._2))

  def merge(otherQuiz: QuizOfWordMappings): QuizOfWordMappings = {
    val wordMappingGroupsCombined = otherQuiz.wordMappingGroups.foldLeft(wordMappingGroups) {
      (acc, otherWmg) =>
        val wmg = findWordMappingGroup(otherWmg.header)
        val wmgMerged = otherWmg.merge(wmg)
        wordMappingGroups.filterNot(Some(_) == wmg) + wmgMerged
    }
    new QuizOfWordMappings(wordMappingGroupsCombined)
  }
}

object QuizOfWordMappings {

  val l = AppDependencies.logger

  def findWordMappingGroup(wmgs: Set[WordMappingGroup], header: model.QuizGroupHeader):
      Option[WordMappingGroup] =
    wmgs.find(_.header.matches(header))

  def metadataFromCustomFormat(str: String): Set[model.QuizGroupHeader] = {
    val wmgHeadings = str.split("wordMappingGroup").tail
    wmgHeadings.map(model.QuizGroupHeader(_)).toSet
  }

  def fromCustomFormat(str: String): QuizOfWordMappings = {
    var quiz = QuizOfWordMappings()
    val wmgHeadings = str.split("wordMappingGroup").tail

    wmgHeadings.foreach {
      wmgHeading => val wmg: WordMappingGroup = WordMappingGroup(QuizGroupHeader(wmgHeading))
                    quiz = quiz.addWordMappingGroup(wmg)
    }
    quiz
  }

  def demoQuiz(wmgsData: List[String] = demoDataInCustomFormat): QuizOfWordMappings = {
    l.log("Using demo data")
    val wmgs = wmgsData.map(WordMappingGroup.fromCustomFormat(_))
    wmgs.foldLeft(QuizOfWordMappings())((acc, wmg) => acc.addWordMappingGroup(wmg))
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