/*
 * Libanius
 * Copyright (C) 2012-2019 James McCabe <james@oranda.com>
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
import com.oranda.libanius.model.action.serialize.Separator
import com.oranda.libanius.model.quizitem.TextValueOps.TextValue
import scala.collection.immutable.{Stream, Iterable}
import com.oranda.libanius.dependencies.AppDependencyAccess
import scala.collection.mutable
import com.oranda.libanius.model.quizitem.QuizItem
import scala.language.implicitConversions
import com.oranda.libanius.model.quizgroup.{QuizGroupUserData, QuizGroupHeader, QuizGroup}
import com.oranda.libanius.util.CollectionHelpers.GroupByOrderedImplicit

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
        QuizItem(
          wmPair.key,
          value.value,
          UserResponsesAll(value.correctAnswersInARow, value.incorrectAnswers)))

    val quizItems: Stream[QuizItem] = wordMappingPairs.flatMap(makeQuizItems)

    QuizGroup.fromQuizItems(quizItems, userData)
  }
}

object WordMappingGroup extends AppDependencyAccess {

  def fromQuizGroup(header: QuizGroupHeader, quizGroup: QuizGroup): WordMappingGroup = {
    val wordMappingPairs = quizItemsToWordMappingPairs(quizGroup.quizItems)
    WordMappingGroup(header, wordMappingPairs, quizGroup.userData)
  }

  def quizItemsToWordMappingPairs(quizItems: Stream[QuizItem]):
      Stream[WordMappingPair] =
    quizItems.groupByOrdered(_.prompt).map {
        case (prompt: TextValue, quizItems: mutable.LinkedHashSet[QuizItem]) =>
          WordMappingPair(
            prompt.value,
            WordMappingValueSet.createFromQuizItems(quizItems.toList))
    }.toStream

}
