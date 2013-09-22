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
import com.oranda.libanius.dependencies.{AppDependencyAccess}
import scala.collection.mutable.ListBuffer
import scala.util.Try
import com.oranda.libanius.util.{GroupByOrderedImplicit}
import scala.collection.mutable
import com.oranda.libanius.model.quizitem.{Value, TextValue, QuizItem}
import scala.language.implicitConversions

/*
 * A type of QuizGroup where the quiz pairs are mappings from a word to possible translations.
 */
case class WordMappingGroup(
    val header: QuizGroupHeader,
    val wordMappingPairs: Stream[WordMappingPair] = Stream.empty) extends ModelComponent {

  def toQuizGroup: QuizGroup = {
    def makeQuizItems(wmPair: WordMappingPair): Iterable[QuizItem] =
      wmPair.valueSet.values.map(value =>
        QuizItem(TextValue(wmPair.key), TextValue(value.value),
            UserResponses(value.correctAnswersInARow, value.incorrectAnswers)))

    val quizItems: Stream[QuizItem] = wordMappingPairs.flatMap(makeQuizItems(_))
    val dictionary = Dictionary.fromWordMappings(wordMappingPairs)
    QuizGroup(quizItems, 0, dictionary)
  }

  def findValueSetFor(key: String): Option[WordMappingValueSet] =
    wordMappingPairs.find(_.key == key).map(_.valueSet)

  def quizKeys = wordMappingPairs.map(_.key)

  def keyBeginningWith(keyStart: String) = quizKeys.find(_.startsWith(keyStart))

}

object WordMappingGroup extends AppDependencyAccess {

  def fromQuizGroup(header: QuizGroupHeader, quizGroup: QuizGroup): WordMappingGroup = {

    // Get access to the groupByOrdered functor
    implicit def traversableToGroupByOrderedImplicit[A](t: Traversable[A]):
        GroupByOrderedImplicit[A] =
      new GroupByOrderedImplicit[A](t)

    val wordMappingPairs: Stream[WordMappingPair] =
      (quizGroup.quizItems.groupByOrdered(_.prompt).map {
        case (prompt: TextValue, quizItems: mutable.LinkedHashSet[QuizItem]) =>
            WordMappingPair(prompt.value, WordMappingValueSet.createFromQuizItems(quizItems.toList))
      }).toStream
    WordMappingGroup(header, wordMappingPairs)
  }

  /*
   * Example:
   *
   * quizGroup type="WordMapping" promptType="English word" responseType="German word" currentPromptNumber="0"
   *    against|wider
   *    entertain|unterhalten
   */
  def fromCustomFormat(str: String): WordMappingGroup = {

    val splitterLineBreak = stringSplitterFactory.getSplitter('\n')
    val splitterKeyValue = stringSplitterFactory.getSplitter('|')

    val wordMappingsMutable = new ListBuffer[WordMappingPair]()

    def parseQuizGroup {
      splitterLineBreak.setString(str)
      splitterLineBreak.next // skip the first line, which has already been parsed

      while (splitterLineBreak.hasNext) {
        val strKeyValue = splitterLineBreak.next
        splitterKeyValue.setString(strKeyValue)

        if (splitterKeyValue.hasNext) {

          def parseKeyValue {
            val strKey = splitterKeyValue.next
            if (splitterKeyValue.hasNext) {
              val strValues = splitterKeyValue.next
              wordMappingsMutable += WordMappingPair(strKey,
                  WordMappingValueSetLazyProxy(strValues))
            }
          }
          Try(parseKeyValue) recover {
            case e: Exception => l.logError("could not parse prompt-response string: " +
                strKeyValue)
          }
        }
      }
    }
    Try(parseQuizGroup) recover {
      case e: Exception => l.logError("could not parse wmg with str " + str.take(100) + "..." +
        str.takeRight(100))
    }

    val wordMappingsStream = wordMappingsMutable.toStream

    // Now use the persistent data structure.
    new WordMappingGroup(QuizGroupHeader(str), wordMappingsStream)
  }
}
