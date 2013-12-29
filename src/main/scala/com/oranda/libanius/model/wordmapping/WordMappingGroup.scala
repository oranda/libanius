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

package com.oranda.libanius.model.wordmapping

import com.oranda.libanius.model._
import scala.collection.immutable.{Stream, Iterable}
import com.oranda.libanius.dependencies.AppDependencyAccess
import scala.collection.mutable.ListBuffer
import scala.util.Try
import com.oranda.libanius.util.{GroupByOrderedImplicit}
import scala.collection.mutable
import com.oranda.libanius.model.quizitem.{TextValue, QuizItem}
import scala.language.implicitConversions
import com.oranda.libanius.model.quizgroup.{QuizGroupUserData, QuizGroupHeader, QuizGroup}

/*
 * An intermediate data structure used to persist a "WordMapping" type of quiz group
 * in a concise format.
 */
case class WordMappingGroup(header: QuizGroupHeader,
    wordMappingPairs: Stream[WordMappingPair] = Stream.empty,
    userData: QuizGroupUserData = new QuizGroupUserData()) extends ModelComponent {

  def toQuizGroup: QuizGroup = {
    def makeQuizItems(wmPair: WordMappingPair): Iterable[QuizItem] =
      wmPair.valueSet.values.map(value =>
        QuizItem(TextValue(wmPair.key), TextValue(value.value),
            UserResponses(value.correctAnswersInARow, value.incorrectAnswers)))

    val quizItems: Stream[QuizItem] = wordMappingPairs.flatMap(makeQuizItems(_))

    QuizGroup(quizItems, userData, new Dictionary())
  }
}

object WordMappingGroup extends AppDependencyAccess {

  def fromQuizGroup(header: QuizGroupHeader, quizGroup: QuizGroup): WordMappingGroup = {
    val wordMappingPairs = quizItemsToWordMappingPairs(quizGroup.quizItems, header.mainSeparator)
    WordMappingGroup(header, wordMappingPairs, quizGroup.userData)
  }

  def quizItemsToWordMappingPairs(quizItems: Stream[QuizItem], mainSeparator: String):
      Stream[WordMappingPair] = {
    // Get access to the groupByOrdered functor
    implicit def traversableToGroupByOrderedImplicit[A](t: Traversable[A]):
        GroupByOrderedImplicit[A] =
      new GroupByOrderedImplicit[A](t)

    (quizItems.groupByOrdered(_.prompt).map {
        case (prompt: TextValue, quizItems: mutable.LinkedHashSet[QuizItem]) =>
          WordMappingPair(prompt.value,
            WordMappingValueSet.createFromQuizItems(quizItems.toList, mainSeparator))
    }).toStream
  }

  /*
   * Example:
   *
   * quizGroup type="WordMapping" promptType="English word" responseType="German word" currentPromptNumber="0" isActive="true"
   *     against|wider
   *     entertain|unterhalten
   */
  def fromCustomFormat(str: String, mainSeparator: String): WordMappingGroup = {

    val splitterLineBreak = stringSplitterFactory.getSplitter('\n')

    val wordMappingsMutable = new ListBuffer[WordMappingPair]()

    def parseQuizGroup {
      splitterLineBreak.setString(str)
      splitterLineBreak.next // skip the first line, which has already been parsed

      while (splitterLineBreak.hasNext) {
        val strPromptResponse = splitterLineBreak.next

        def parsePromptResponse = {

          val i = strPromptResponse.indexOf(mainSeparator)
          val strPrompt = strPromptResponse.substring(0, i).trim
          val strResponseAndUserInfo = strPromptResponse.substring(i + mainSeparator.length)
          wordMappingsMutable += WordMappingPair(strPrompt,
            WordMappingValueSetLazyProxy(strResponseAndUserInfo, mainSeparator))
        }

        Try(parsePromptResponse) recover {
            case e: Exception => l.logError("could not parse prompt-response string: " +
                strPromptResponse)
        }
      }
    }
    Try(parseQuizGroup) recover {
      case e: Exception => l.logError("could not parse wmg with str " + str.take(100) + "..." +
          str.takeRight(100))
    }

    val wordMappingsStream = wordMappingsMutable.toStream

    // Now use the persistent data structure.
    new WordMappingGroup(QuizGroupHeader(str), wordMappingsStream, QuizGroupUserData(str))
  }
}
