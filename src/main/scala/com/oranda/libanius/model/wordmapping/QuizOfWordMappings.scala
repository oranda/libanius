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
import com.oranda.libanius.util.{Platform}
import com.oranda.libanius.{Conf}

import scala.math.BigDecimal.double2bigDecimal
import scala.collection.immutable.List
import scala.collection.immutable.Iterable

case class QuizOfWordMappings(wordMappingGroups: Set[WordMappingGroupReadWrite] = ListSet())
    extends Quiz with Platform {

  def copy(newPromptNumber: Int) = new QuizOfWordMappings(wordMappingGroups)

  def wmgToCustomFormat(strBuilder: StringBuilder, wmg: WordMappingGroupReadWrite) = 
    wmg.toCustomFormat(strBuilder)
    
  def findOrAddWordMappingGroup(keyType: String, valueType: String): 
      (QuizOfWordMappings, WordMappingGroupReadWrite) = {
    val wordMappingGroup = findWordMappingGroup(keyType, valueType)    
    wordMappingGroup match {
      case Some(wordMappingGroup) => (this, wordMappingGroup)
      case None => val wordMappingGroup = WordMappingGroupReadWrite(keyType, valueType)
                   (addWordMappingGroup(wordMappingGroup), wordMappingGroup)
    }    
  }
     
  def findValuesFor(keyWord: String, keyType: String, valueType: String): 
      Iterable[String] = {
    findWordMappingGroup(keyType, valueType).foreach(
      _.findValueSetFor(keyWord).foreach(wordMappingValueSet =>
        return wordMappingValueSet.strings.toList        
    ))    
    new HashSet[String]
  }

  def findWordMappingGroup(keyType: String, valueType: String): Option[WordMappingGroupReadWrite] =
    QuizOfWordMappings.findWordMappingGroup(wordMappingGroups, keyType, valueType)
  
  def removeWordMappingGroup(keyType: String, valueType: String): QuizOfWordMappings = {
    //val wordMappingGroup = findWordMappingGroup(keyType, valueType)
    val wordMappingGroupsFiltered = wordMappingGroups.filterNot(
        WordMappingGroup.matches(_, keyType, valueType))
    new QuizOfWordMappings(wordMappingGroupsFiltered)
  }

  // This will replace any existing wordMappingGroup with the same key-value pair  
  def addWordMappingGroup(wmg: WordMappingGroupReadWrite): QuizOfWordMappings = {
    val newQuiz = removeWordMappingGroup(wmg.keyType, wmg.valueType)
    new QuizOfWordMappings(newQuiz.wordMappingGroups + wmg)
  }

  // Just a synonym for addWordMappingGroup
  def replaceWordMappingGroup(wmg: WordMappingGroupReadWrite) = addWordMappingGroup(wmg)

  def removeWord(keyWord: String, keyType: String, valueType: String): QuizOfWordMappings = {
    val wordMappingGroup = findWordMappingGroup(keyType, valueType)
    wordMappingGroup match {
      case Some(wmg) => replaceWordMappingGroup(wmg.removeWordMapping(keyWord))
      case None => this
    }
  }
  
  def removeWordMappingValue(keyWord: String, wordMappingValue: WordMappingValue,
      keyType: String, valueType: String): (QuizOfWordMappings, Boolean) = {
    val wordMappingGroup = findWordMappingGroup(keyType, valueType)
    wordMappingGroup match {
      case Some(wmg) =>
        val (newWmg, wasRemoved) = wmg.removeWordMappingValue(keyWord, wordMappingValue)
        (replaceWordMappingGroup(newWmg), wasRemoved)
      case None => (this, false)
    }
  }

  def findQuizItem: Option[(QuizItemViewWithOptions, WordMappingGroupReadWrite)] =
    /*
     * Find the first available "presentable" word mapping
     */
    (for {
      wmg <- wordMappingGroups.toStream
      quizItem <- wmg.findPresentableQuizItem.toStream
    } yield (quizItem, wmg)).headOption.orElse(findAnyUnfinishedQuizItem)

  def findAnyUnfinishedQuizItem: Option[(QuizItemViewWithOptions, WordMappingGroupReadWrite)] =
    (for {
      wmg <- wordMappingGroups.toStream
      quizItem <- wmg.findAnyUnfinishedQuizItem.toStream
    } yield (quizItem, wmg)).headOption

  def addWordMappingToFront(keyType: String, valueType: String,
                            keyWord: String, value: String): QuizOfWordMappings = {
    val wmg = findWordMappingGroup(keyType, valueType)
    wmg match {
      case Some(wmg) => replaceWordMappingGroup(wmg.addWordMappingToFront(keyWord, value))
      case None => this
    }
  }

  def addWordMappingToFrontOfTwoGroups(keyType: String, valueType: String, 
      keyWord: String, value: String): QuizOfWordMappings = {

    // E.g. add to the English -> German group
    val quizAfter1stChange = addWordMappingToFront(keyType, valueType, keyWord, value)
    
    // E.g. add to the German -> English group
    val quizAfter2ndChange = quizAfter1stChange.addWordMappingToFront(
        valueType, keyType, value, keyWord)
    
    quizAfter2ndChange
  }

  def updateWithUserAnswer(isCorrect: Boolean, currentQuizItem: QuizItemViewWithOptions):
      QuizOfWordMappings = {
    val userAnswer = new UserAnswer(isCorrect, currentQuizItem.wmgCurrentPromptNumber)
    val wmg = findWordMappingGroup(currentQuizItem.keyType, currentQuizItem.valueType)

    val wmgUpdated = wmg.get.updateWithUserAnswer(currentQuizItem.keyWord,
        currentQuizItem.wmvs, currentQuizItem.wordMappingValue, userAnswer)
    addWordMappingGroup(wmgUpdated)
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
  def updateRangeForFailedWmgs(wmgSuccessful: WordMappingGroupReadWrite): QuizOfWordMappings = {
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
        val wmg = findWordMappingGroup(otherWmg.keyType, otherWmg.valueType)
        val wmgMerged = otherWmg.merge(wmg)
        wordMappingGroups.filterNot(Some(_) == wmg) + wmgMerged
    }      
    new QuizOfWordMappings(wordMappingGroupsCombined)
  }
}

object QuizOfWordMappings extends Platform {

  def findWordMappingGroup(wmgs: Set[WordMappingGroupReadWrite], keyType: String, valueType: String):
      Option[WordMappingGroupReadWrite] =
    wmgs.find(WordMappingGroup.matches(_, keyType, valueType))

  def fromCustomFormat(str: String, wmgs: Set[WordMappingGroupReadWrite]): QuizOfWordMappings = {
    
    var quiz = QuizOfWordMappings()
    
    val wmgHeadings = str.split("wordMappingGroup").tail

    def findWmg(wmgHeading: String, wmgs: Set[WordMappingGroupReadWrite]):
        Option[WordMappingGroupReadWrite] = {
      val keyType = WordMappingGroup.parseKeyType(wmgHeading)
      val valueType = WordMappingGroup.parseValueType(wmgHeading)
      findWordMappingGroup(wmgs, keyType, valueType)
    }

    wmgHeadings.foreach { wmgHeading =>
      findWmg(wmgHeading, wmgs).foreach(wmg => quiz = quiz.addWordMappingGroup(wmg))
    }
    quiz
  }


  def demoQuiz(wmgsData: List[String] = demoDataInCustomFormat): QuizOfWordMappings = {
    log("Libanius", "Using demo data")
    val wmgs = wmgsData.map(WordMappingGroupReadWrite.fromCustomFormat(_))
    wmgs.foldLeft(QuizOfWordMappings())((acc, wmg) => acc.addWordMappingGroup(wmg))
    // TODO: watch out when we're saving, we're not overwriting anything
  }

  // Demo data to use as a fallback if no file is available
  def demoDataInCustomFormat = List(

    "wordMappingGroup keyType=\"English word\" valueType=\"German word\"\n" +
    "en route|unterwegs\n" +
    "contract|Vertrag\n" +
    "treaty|Vertrag\n" +
    "against|wider\n" +
    "entertain|unterhalten\n",

    "wordMappingGroup keyType=\"German word\" valueType=\"English word\"\n" +
    "unterwegs|en route\n" +
    "Vertrag|contract/treaty\n" +
    "wider|against\n" +
    "unterhalten|entertain")

}