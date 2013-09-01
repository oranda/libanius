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
import scala.util.{Try, Random}

import com.oranda.libanius.util.{StringUtil, Util}
import com.oranda.libanius.dependencies.AppDependencies
import com.oranda.libanius.model.wordmapping.{WordMappingValue, WordMappingGroup, Dictionary}

case class QuizGroup(
    header: QuizGroupHeader,
    quizItems: Stream[QuizItem] = Stream.empty,
    currentPromptNumber: Int = 0,
    dictionary: Dictionary = new Dictionary)
  extends ModelComponent {

  private[this] lazy val l = AppDependencies.logger

  def promptType = header.promptType           // example: "English word"
  def responseType = header.responseType // example: "German word"

  def updatedQuizItems(newQuizItems: Stream[QuizItem]): QuizGroup =
    new QuizGroup(header, newQuizItems, currentPromptNumber, dictionary)

  def updatedPromptNumber: QuizGroup =
    new QuizGroup(header, quizItems, currentPromptNumber + 1, dictionary)


  def updatedWithUserAnswer(prompt: Value, response: Value, wasCorrect: Boolean,
      userResponses: UserResponses, userAnswer: UserResponse) = {
    val userResponseUpdated = userResponses.addUserAnswer(userAnswer, wasCorrect)
    addQuizItem(prompt, response, userResponseUpdated)
  }

  def addQuizItem(prompt: Value, response: Value, userResponses: UserResponses = UserResponses()):
      QuizGroup =
    addQuizItemToFront(QuizItem(prompt, response, userResponses))

  def addNewQuizItem(prompt: String, response: String): QuizGroup =
    if (!prompt.isEmpty && !response.isEmpty && prompt.toLowerCase != response.toLowerCase)
      addQuizItemToFront(QuizItem(prompt, response))
    else
      this

  protected[model] def addQuizItemToFront(key: Value, value: Value): QuizGroup =
    addQuizItemToFront(QuizItem(key, value))

  protected[model] def addQuizItemToFront(quizItem: QuizItem): QuizGroup =
    addQuizItemToFront(quizItems, quizItem)

  protected[model] def addQuizItemToFront(quizItems: Stream[QuizItem],
      quizItem: QuizItem): QuizGroup =
    updatedQuizItems(quizItem +: remove(quizItem))

  protected[model] def addQuizItemToEnd(quizItem: QuizItem): QuizGroup =
    addQuizItemToEnd(quizItems, quizItem)

  protected[model] def addQuizItemToEnd(quizItems: Stream[QuizItem], quizItem: QuizItem):
      QuizGroup =
    updatedQuizItems(remove(quizItem) :+ quizItem)

  def removeQuizItem(quizItem: QuizItem) = updatedQuizItems(remove(quizItem))

  private def remove(quizItem: QuizItem): Stream[QuizItem] =
    quizItems.filterNot(_.samePromptAndResponse(quizItem))

  def removeQuizItemsForPrompt(prompt: String) =
    updatedQuizItems(quizItems.filter(_.prompt.text != prompt))
  def removeQuizItemsForResponse(response: String) =
    updatedQuizItems(quizItems.filter(_.response.text != response))

  def updatedDictionary(newDictionary: Dictionary) =
    new QuizGroup(header, quizItems, currentPromptNumber, newDictionary)

  def quizPrompts: Stream[Value] = quizItems.map(_.prompt)
  def quizResponses: Stream[Value] = quizItems.map(_.response)

  def matches(qgOther: QuizGroup): Boolean = header.matches(qgOther.header)

  /*
   * Example of custom format:
   *
   * quizGroup type="WordMapping" promptType="English word" responseType="German word" currentPromptNumber="0"
   *    against|wider
   *    entertain|unterhalten
   */
  def toCustomFormat(strBuilder: StringBuilder) = {
    val wordMappingGroup = WordMappingGroup.fromQuizGroup(this)
    header.toCustomFormat(strBuilder).append(" currentPromptNumber=\"").
        append(currentPromptNumber).append("\"")
    // Imperative code is used for speed
    val iter = wordMappingGroup.wordMappingPairs.iterator
    while (iter.hasNext) {
      val quizItem = iter.next
      strBuilder.append('\n').append(quizItem.key).append('|')
      quizItem.valueSet.toCustomFormat(strBuilder)
    }
    strBuilder
  }

  def getSaveData: SaveData = {
    val serialized = toCustomFormat(new StringBuilder())
    val fileName = header.makeQgFileName
    SaveData(fileName, serialized.toString)
  }

  def contains(prompt: String): Boolean = contains(TextValue(prompt))
  def contains(prompt: Value): Boolean = quizPrompts.contains(prompt)
  def size = quizItems.size
  def numPrompts = size
  def numResponses = size
  def numCorrectAnswers: Int = quizItems.map(_.userResponses.numCorrectAnswersInARow).sum


  // Low usage expected. Slow because we are not using a Map for quizItems.
  def findValuesFor(prompt: String): List[String] =
    quizItems.filter(_.prompt.text == prompt).map(_.response.text).toList

  def findAnyUnfinishedQuizItem: Option[QuizItemViewWithChoices] = {
    l.log("findAnyUnfinishedQuizItem " + header)
    quizItems.iterator.find(_.userResponses.isUnfinished).map(
        pair => quizItemWithOptions(pair, pair.response.text))
  }

  protected def quizItemWithOptions(quizItem: QuizItem,
      quizValueCorrect: String): QuizItemViewWithChoices = {
    val numCorrectAnswers = quizItem.userResponses.numCorrectAnswersInARow
    val falseAnswers = makeFalseAnswers(quizItem, quizValueCorrect, numCorrectAnswers)
    new QuizItemViewWithChoices(quizItem, currentPromptNumber, header, falseAnswers,
        numCorrectAnswers)
  }


  protected def findPresentableQuizItem(quizItem: QuizItem, currentPromptNumber: Int):
      Option[QuizItemViewWithChoices] = {
    if (quizItem.isPresentable(currentPromptNumber))
      Util.stopwatch(Some(quizItemWithOptions(quizItem, quizItem.response.text)),
          "quizItemWithOptions for " + quizItem.response)
    else
      None
  }

  def makeFalseAnswers(wmpCorrect: QuizItem, quizValueCorrect: String,
      numCorrectAnswersSoFar: Int): Set[String] = {

    var falseAnswers = new ListSet[String]
    val numFalseAnswersRequired = 2

    /*
     * If the user has already been having success with this word, first try to
     * fill the falseAnswers with similar-looking words.
     */
    if (numCorrectAnswersSoFar >= 1) {
      val correctValues = findValuesFor(wmpCorrect.prompt.text)
      falseAnswers ++= Util.stopwatch(makeFalseSimilarAnswers(correctValues,
          quizValueCorrect, numCorrectAnswersSoFar, numFalseAnswersRequired),
          "makeFalseSimilarAnswers")
    }

    // try again to fill the falseAnswers
    var totalTries = 20 // to stop any infinite loop
    while (falseAnswers.size < numFalseAnswersRequired && totalTries > 0) {
      totalTries = totalTries - 1
      val randomAnswer: Option[String] = findRandomWordValue(randomValues(100))
      randomAnswer.foreach( randomAnswer =>
        if (wmpCorrect.response.text != randomAnswer)
          falseAnswers += randomAnswer
      )
    }

    // final try to fill false answers: use dummy data
    if (falseAnswers.isEmpty) falseAnswers += ""
    while (falseAnswers.size < numFalseAnswersRequired)
      falseAnswers += (falseAnswers.last + " ")

    falseAnswers
  }


  def makeFalseSimilarAnswers(correctValues: List[String], correctValue: String,
      numCorrectAnswersSoFar: Int, numFalseAnswersRequired: Int): Set[String] = {

    var similarWords = new HashSet[String]

    def hasSameStart = (value1: Value, value2: String) => value1.hasSameStart(value2)
    def hasSameEnd = (value1: Value, value2: String) => value1.hasSameEnd(value2)
    val similarityFunction = if (numCorrectAnswersSoFar % 2 == 1) hasSameStart else hasSameEnd

    var numValueSetsSearched = 0
    val numSimilarLettersRequired = 2
    quizItems.iterator.takeWhile(_ => similarWords.size < numFalseAnswersRequired).
        foreach(quizItem => {
      numValueSetsSearched = numValueSetsSearched + 1
      // Avoid selecting values belonging to the "correct" response set
      if (!correctValues.contains(quizItem.response)) {
        if (similarWords.size < numFalseAnswersRequired &&
            similarityFunction(quizItem.response, correctValue)(numSimilarLettersRequired))
          similarWords += quizItem.response.text
      }
    })
    similarWords
  }

  def findRandomWordValue(quizValues: Seq[Value]): Option[String] = {
    if (quizValues.isEmpty)
      None
    else {
      val randomIndex = Random.nextInt(quizValues.length)
      Some(quizValues(randomIndex).text)
    }
  }

  def merge(otherWmg: Option[QuizGroup]): QuizGroup =
    otherWmg.map(merge(_)).getOrElse(this)

  def merge(otherWmg: QuizGroup): QuizGroup = {
    val quizItemsCombined = (quizItems ++ otherWmg.quizItems).asInstanceOf[Stream[QuizItem]]
    updatedQuizItems(quizItemsCombined)
  }

  def hasPrompt(prompt: String): Boolean = quizPrompts.contains(prompt)
  //def promptBeginningWith(promptStart: String): Boolean = quizPrompts.find(_.hasSameStart(promptStart))


  def findPresentableQuizItem: Option[QuizItemViewWithChoices] = {
    val quizItem =
      (for {
        quizItem <- quizItems.toStream
        quizItem <- findPresentableQuizItem(quizItem, currentPromptNumber)
      } yield quizItem).headOption
    l.log("found quiz item " + quizItem)
    quizItem
  }


  def randomValues(sliceSize: Int): List[Value] =
    randomSliceOfQuizItems(sliceSize).map(_.response).toList

  def randomSliceOfQuizItems(sliceSize: Int): Iterable[QuizItem] =
    if (sliceSize >= size) quizItems.toList
    else {
      val randomStart = Random.nextInt(size - sliceSize)
      quizItems.slice(randomStart, randomStart + sliceSize)
    }

}


object QuizGroup {

  private[this] val l = AppDependencies.logger

  def parseCurrentPromptNumber(str: String): Int =
    Try(StringUtil.parseValue(str, "currentPromptNumber=\"", "\"").toInt).recover {
      case e: Exception => l.logError("Could not parse prompt number from " + str)
                           0
    }.get

  /*
  def apply(header: QuizGroupHeader): QuizGroup = {
    // TODO: fix this
    header.quizGroupType match {
      case WordMapping => QuizGroup(header)
      case QuestionAndAnswer => QuizGroup(header)
    }
  } */

  // TODO: improve... is it really necessary to go through WordMappingGroup?
  def fromCustomFormat(text: String): QuizGroup =
    WordMappingGroup.fromCustomFormat(text).toQuizGroup

}