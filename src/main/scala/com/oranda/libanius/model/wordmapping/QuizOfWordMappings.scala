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

import com.oranda.libanius.model.{UserAnswer, Quiz}
import com.oranda.libanius.util.{StringUtil, Platform}
import com.oranda.libanius.{Conf}

import scala.math.BigDecimal.double2bigDecimal
import scala.collection.immutable.List

case class QuizOfWordMappings(wordMappingGroups: Set[WordMappingGroup] = ListSet())
    extends Quiz with Platform {

  def copy(newPromptNumber: Int) = new QuizOfWordMappings(wordMappingGroups)

  /*
   * This serialization does not include wordMappingGroup (wmg) data, only metadata.
   *
   * Example:
   * quizOfWordMappings
   * wordMappingGroup keyType="German word" valueType="English word"
   * wordMappingGroup keyType="English word" valueType="German word"
   */
  def toCustomFormat: StringBuilder = {
    // For efficiency, avoiding Scala's own StringBuilder and mkString
    val strBuilder = new StringBuilder("quizOfWordMappings\n")

    def wmgMetadata(strBuilder: StringBuilder, wmg: WordMappingGroup) =
      strBuilder.append("wordMappingGroup keyType=\"").append(wmg.keyType).
          append("\" valueType=\"").append(wmg.valueType).append("\"")

    StringUtil.mkString(strBuilder, wordMappingGroups, wmgMetadata, '\n')
  }

  def findOrAddWordMappingGroup(header: QuizGroupHeader):
      (QuizOfWordMappings, WordMappingGroup) = {
    val wordMappingGroup = findWordMappingGroup(header)
    wordMappingGroup match {
      case Some(wordMappingGroup) => (this, wordMappingGroup)
      case None => val wordMappingGroup = WordMappingGroup(header)
                   (addWordMappingGroup(wordMappingGroup), wordMappingGroup)
    }    
  }
     
  def findValuesFor(keyWord: String, header: QuizGroupHeader): List[String] = {
    log("Looking for values for " + keyWord + " in " + header)

    findWordMappingGroup(header) match {
      case Some(quizGroup) => quizGroup.findValueSetFor(keyWord) match {
        case Some(wordMappingValueSet) => wordMappingValueSet.strings.toList
        case _ => log("quizGroup numKeyWords = " + quizGroup.numKeyWords)
            log("quizGroup first 10 wordMappings: " + quizGroup.wordMappings.take(10))
            logError("could not find valueSet for keyWord " + keyWord)
            Nil
      }
      case _ => logError("could not find quizGroup for " + header)
          Nil
    }
  }

  def findWordMappingGroup(header: QuizGroupHeader): Option[WordMappingGroup] =
    QuizOfWordMappings.findWordMappingGroup(wordMappingGroups, header)
  
  def removeWordMappingGroup(header: QuizGroupHeader): QuizOfWordMappings = {
    //val wordMappingGroup = findWordMappingGroup(keyType, valueType)
    val wordMappingGroupsFiltered = wordMappingGroups.filterNot(
        _.header.matches(header))
    new QuizOfWordMappings(wordMappingGroupsFiltered)
  }

  // This will replace any existing wordMappingGroup with the same key-value pair  
  def addWordMappingGroup(wmg: WordMappingGroup): QuizOfWordMappings = {
    val newQuiz = removeWordMappingGroup(wmg.header)
    new QuizOfWordMappings(newQuiz.wordMappingGroups + wmg)
  }

  // Just a synonym for addWordMappingGroup
  def replaceWordMappingGroup(wmg: WordMappingGroup) = addWordMappingGroup(wmg)

  def removeWord(keyWord: String, header: QuizGroupHeader): QuizOfWordMappings = {
    val wordMappingGroup = findWordMappingGroup(header)
    wordMappingGroup match {
      case Some(wmg) => replaceWordMappingGroup(wmg.removeWordMapping(keyWord))
      case None => this
    }
  }
  
  def removeWordMappingValue(keyWord: String, wordMappingValue: WordMappingValue,
      header: QuizGroupHeader): (QuizOfWordMappings, Boolean) = {
    val wordMappingGroup = findWordMappingGroup(header)
    wordMappingGroup match {
      case Some(wmg) =>
        val (newWmg, wasRemoved) = wmg.removeWordMappingValue(keyWord, wordMappingValue)
        (replaceWordMappingGroup(newWmg), wasRemoved)
      case None => (this, false)
    }
  }

  def findQuizItem: Option[(QuizItemViewWithOptions, WordMappingGroup)] =
    /*
     * Find the first available "presentable" word mapping
     */
    (for {
      wmg <- wordMappingGroups.toStream
      quizItem <- wmg.findPresentableQuizItem.toStream
    } yield (quizItem, wmg)).headOption.orElse(findAnyUnfinishedQuizItem)

  def findAnyUnfinishedQuizItem: Option[(QuizItemViewWithOptions, WordMappingGroup)] =
    (for {
      wmg <- wordMappingGroups.toStream
      quizItem <- wmg.findAnyUnfinishedQuizItem.toStream
    } yield (quizItem, wmg)).headOption

  def addWordMappingToFront(header: QuizGroupHeader, keyWord: String, value: String):
      QuizOfWordMappings = {
    val wmg = findWordMappingGroup(header)
    wmg match {
      case Some(wmg) => replaceWordMappingGroup(wmg.addWordMappingToFront(keyWord, value))
      case None => this
    }
  }

  def addWordMappingToFrontOfTwoGroups(header: QuizGroupHeader,
      keyWord: String, value: String): QuizOfWordMappings = {

    log("Adding to 2 wmgs: " + keyWord + "," + value)
    // E.g. add to the English -> German group
    val quizUpdated1 = addWordMappingToFront(header, keyWord, value)
    
    // E.g. add to the German -> English group
    val quizUpdated2 = quizUpdated1.addWordMappingToFront(header.reverse, value, keyWord)

    quizUpdated2
  }

  def updateWithUserAnswer(isCorrect: Boolean, currentQuizItem: QuizItemViewWithOptions):
      QuizOfWordMappings = {
    val userAnswer = new UserAnswer(isCorrect, currentQuizItem.wmgCurrentPromptNumber)
    val wmg = findWordMappingGroup(currentQuizItem.quizGroupHeader)
    wmg match {
      case Some(wmg) =>
        val wmgUpdated = wmg.updateWithUserAnswer(currentQuizItem.keyWord,
            currentQuizItem.wmvs, currentQuizItem.wordMappingValue, userAnswer)
        addWordMappingGroup(wmgUpdated)
      case _ => this
    }
  }

  /*
   * Return a quiz with wmg's updated to have changed "search ranges" where a search just failed.
   *
   * This assumes there has just been a search through the list of wmg's for quiz items,
   * and one of the wmg's returned something successfully. This means that all of the wmg's
   * prior to that (if any) "failed" and the search range within each should be shifted to improve
   * the chances of a hit next time.
   * Note: this design is not great, and will be improved when each wmg is an independent Actor.
   */
  def updateRangeForFailedWmgs(wmgSuccessful: WordMappingGroup): QuizOfWordMappings = {
    val wmgsFailed = wordMappingGroups.takeWhile(!_.matches(wmgSuccessful))
    val wmgsWithUpdatedRange = wmgsFailed.map(_.updatedSearchRange)
    wmgsWithUpdatedRange.foldLeft(this)((acc, wmg) => acc.addWordMappingGroup(wmg))
  }

  def numGroups = wordMappingGroups.size
  
  def numKeyWords = wordMappingGroups.foldLeft(0)(_ + _.numKeyWords)
  
  def numItems: Int = wordMappingGroups.foldLeft(0)(_ + _.numValues)
  
  override def scoreSoFar: BigDecimal = {  // out of 1.0
    val _numItemsAndCorrectAnswers = numItemsAndCorrectAnswers
    val scoreSoFar = _numItemsAndCorrectAnswers._2.toDouble / 
        (_numItemsAndCorrectAnswers._1 * Conf.conf.numCorrectAnswersRequired).toDouble
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

object QuizOfWordMappings extends Platform {

  def findWordMappingGroup(wmgs: Set[WordMappingGroup], header: QuizGroupHeader):
      Option[WordMappingGroup] =
    wmgs.find(_.header.matches(header))

  def metadataFromCustomFormat(str: String): Set[QuizGroupHeader] = {
    val wmgHeadings = str.split("wordMappingGroup").tail
    wmgHeadings.map(QuizGroupHeader(_)).toSet
  }

  def fromCustomFormat(str: String): QuizOfWordMappings = {
    
    var quiz = QuizOfWordMappings()
    
    val wmgHeadings = str.split("wordMappingGroup").tail

    wmgHeadings.foreach {
      wmgHeading => quiz = quiz.addWordMappingGroup(
          WordMappingGroup(QuizGroupHeader(wmgHeading)))
    }
    quiz
  }


  def demoQuiz(wmgsData: List[String] = demoDataInCustomFormat): QuizOfWordMappings = {
    log("Using demo data")
    val wmgs = wmgsData.map(WordMappingGroup.fromCustomFormat(_))
    wmgs.foldLeft(QuizOfWordMappings())((acc, wmg) => acc.addWordMappingGroup(wmg))
    // TODO: watch out when we're saving, we're not overwriting anything
  }

  // Demo data to use as a fallback if no file is available
  def demoDataInCustomFormat = List(

    "wordMappingGroup keyType=\"English word\" valueType=\"German word\" currentPromptNumber=\"0\"\n" +
    "en route|unterwegs\n" +
    "contract|Vertrag\n" +
    "treaty|Vertrag\n" +
    "against|wider\n" +
    "entertain|unterhalten\n",

    "wordMappingGroup keyType=\"German word\" valueType=\"English word\" currentPromptNumber=\"0\"\n" +
    "unterwegs|en route\n" +
    "Vertrag|contract/treaty\n" +
    "wider|against\n" +
    "unterhalten|entertain")

}