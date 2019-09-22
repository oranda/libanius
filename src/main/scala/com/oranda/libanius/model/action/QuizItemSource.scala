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

package com.oranda.libanius.model.action

import com.oranda.libanius.model.quizitem.{QuizItemViewWithChoices, QuizItem}
import com.oranda.libanius.model.quizgroup.{QuizGroupMemoryLevel, QuizGroup}
import com.oranda.libanius.dependencies.AppDependencyAccess
import com.oranda.libanius.model.ModelComponent
import com.oranda.libanius.model.Quiz


/**
 * Type class definition for finding quiz items in model entities.
 */
trait QuizItemSourceBase[A <: ModelComponent, B <: Params, C] {
  def produceQuizItem(component: A, params: B): Option[C]
  def findAnyUnfinishedQuizItem(component: A, params:B): Option[C]
}

/**
 * In the normal case of QuizItemSource, there are no extra parameters needed.
 */
trait QuizItemSource[A <: ModelComponent, C] extends QuizItemSourceBase[A, NoParams, C]

// provides external access to the typeclass, forwarding the call to the appropriate type
object QuizItemSource {

  def produceQuizItem[A <: ModelComponent, B <: Params, C]
      (component: A, params: B)
      (implicit qis: QuizItemSourceBase[A, B, C], c: C => QuizItem): Option[C] =
    qis.produceQuizItem(component, params)

  def findAnyUnfinishedQuizItem[A <: ModelComponent, B <: Params, C]
      (component: A, params: B)
      (implicit qis: QuizItemSourceBase[A, B, C], c: C => QuizItem): Option[C] =
    qis.findAnyUnfinishedQuizItem(component, params)
}

object modelComponentsAsQuizItemSources extends AppDependencyAccess {

  implicit object quizAsSource extends QuizItemSource[Quiz, QuizItemViewWithChoices] {

    /*
     * Find the first available "presentable" quiz item.
     * Return a quiz item view and the associated quiz group header.
     */
    def produceQuizItem(
        quiz: Quiz,
        params: NoParams = NoParams()): Option[QuizItemViewWithChoices] =
      (for {
        (header, quizGroup) <- quiz.activeQuizGroups.toStream
        quizItem <- quizGroupAsSource.produceQuizItem(quizGroup).toStream
      } yield quizGroup.quizItemWithChoices(quizItem, header)).headOption

    /*
     * Near the end of the quiz, there will be items that are "nearly learnt" because they
     * have been answered correctly several times, but are not considered presentable under
     * normal criteria, because the last correct response was recent. However, they do need
     * to be presented in order for the quiz to finish, so this method is called as a last try.
     */
    def findAnyUnfinishedQuizItem(
        quiz: Quiz,
        params: NoParams = NoParams()): Option[QuizItemViewWithChoices] = {
      l.log("calling Quiz.findAnyUnfinishedQuizItem")
      (for {
        (header, quizGroup) <- quiz.activeQuizGroups
        quizItem <- quizGroupAsSource.findAnyUnfinishedQuizItem(quizGroup, NoParams())
      } yield quizGroup.quizItemWithChoices(quizItem, header)).headOption
    }
  }

  implicit object quizGroupAsSource extends QuizItemSource[QuizGroup, QuizItem] {
    /*
     * The memLevels are searched in reverse order for presentable quiz items,
     * meaning that an item that has been answered correctly (once or more) will
     * be preferred over an item with no correct answers, assuming the
     * interval criteria (difference with the prompt number) is satisfied.
     */
    def produceQuizItem(qg: QuizGroup, params: NoParams = NoParams()): Option[QuizItem] =
      (for {
        (memLevel, levelIndex) <- qg.levels.zipWithIndex.reverse.tail.toStream
        quizItem <- quizGroupMemoryLevelAsSource.produceQuizItem(memLevel,
            CurrentPromptNumber(qg)).toStream
      } yield quizItem).headOption

    def findAnyUnfinishedQuizItem(qg: QuizGroup, params: NoParams = NoParams()): Option[QuizItem] =
      (for {
        (memLevel, levelIndex) <- qg.levels.zipWithIndex.reverse.tail.toStream
        quizItem <- quizGroupMemoryLevelAsSource.findAnyUnfinishedQuizItem(
            memLevel, CurrentPromptNumber(qg))
      } yield quizItem).headOption
  }

  implicit object quizGroupMemoryLevelAsSource
    extends QuizItemSourceBase[QuizGroupMemoryLevel, CurrentPromptNumber, QuizItem] {

    def produceQuizItem(qgml: QuizGroupMemoryLevel, params: CurrentPromptNumber):
        Option[QuizItem] =
      (for {
        quizItem <- qgml.quizItems.toStream
        if quizItem.isPresentable(params.currentPromptNumber, qgml.repetitionInterval)
      } yield quizItem).headOption

    def findAnyUnfinishedQuizItem(qgml: QuizGroupMemoryLevel, params: CurrentPromptNumber):
        Option[QuizItem] =
      qgml.quizItems.headOption
  }
}

trait Params
case class NoParams() extends Params
case class CurrentPromptNumber(currentPromptNumber: Int) extends Params
/**
 * Provide factory method based on quiz group for CurrentPromptNumber Params.
 */
object CurrentPromptNumber {
  def apply(qg: QuizGroup): CurrentPromptNumber = CurrentPromptNumber(qg.currentPromptNumber)
}
