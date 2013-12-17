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
