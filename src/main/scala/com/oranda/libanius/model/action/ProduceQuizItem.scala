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

package com.oranda.libanius.model.action.producequizitem

import com.oranda.libanius.model.quizitem.{QuizItemViewWithChoices, QuizItem}
import com.oranda.libanius.model.quizgroup.{QuizGroupMemoryLevel, QuizGroup}
import com.oranda.libanius.dependencies.AppDependencyAccess
import com.oranda.libanius.model.ModelComponent
import com.oranda.libanius.model.Quiz

/**
 * Type class definition for finding quiz items in model entities.
 */
trait ProduceQuizItem[A <: ModelComponent, B <: Params, C] {
  def findPresentableQuizItem(component: A, params: B): Option[C]
  def findAnyUnfinishedQuizItem(component: A, params: B): Option[C]
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

// provides external access to the typeclass, forwarding the call to the appropriate type
object ProduceQuizItem {

  def findPresentableQuizItem[A <: ModelComponent, B <: Params, C](component: A, params: B)
      (implicit pqi: ProduceQuizItem[A, B, C], c: C => QuizItem): Option[C] =
    pqi.findPresentableQuizItem(component, params)

  def findAnyUnfinishedQuizItem[A <: ModelComponent, B <: Params, C](component: A, params: B)
      (implicit pqi: ProduceQuizItem[A, B, C], c: C => QuizItem): Option[C] =
    pqi.findAnyUnfinishedQuizItem(component, params)
}

object ProduceQuizItemForModelComponents extends AppDependencyAccess {

  implicit object produceQuizItemQuiz extends ProduceQuizItem[Quiz, NoParams, QuizItemViewWithChoices] {

    /*
     * Find the first available "presentable" quiz item.
     * Return a quiz item view and the associated quiz group header.
     */
    def findPresentableQuizItem(quiz: Quiz, params: NoParams): Option[QuizItemViewWithChoices] =
      (for {
        (header, quizGroup) <- quiz.activeQuizGroups.toStream
        quizItem <- produceQuizItemQuizGroup.findPresentableQuizItem(quizGroup, NoParams()).toStream
      } yield quizGroup.quizItemWithChoices(quizItem, header)).headOption

    /*
     * Near the end of the quiz, there will be items that are "nearly learnt" because they
     * have been answered correctly several times, but are not considered presentable under
     * normal criteria, because the last correct response was recent. However, they do need
     * to be presented in order for the quiz to finish, so this method is called as a last try.
     */
    def findAnyUnfinishedQuizItem(quiz: Quiz, params: NoParams): Option[QuizItemViewWithChoices] = {
      l.log("calling findAnyUnfinishedQuizItem")
      (for {
        (header, quizGroup) <- quiz.activeQuizGroups
        quizItem <- produceQuizItemQuizGroup.findAnyUnfinishedQuizItem(quizGroup, NoParams())
      } yield quizGroup.quizItemWithChoices(quizItem, header)).headOption
    }
  }

  implicit object produceQuizItemQuizGroup extends ProduceQuizItem[QuizGroup, NoParams, QuizItem] {
    /*
     * The memLevels are searched in reverse order for presentable quiz items,
     * meaning that an item that has been answered correctly (once or more) will
     * be preferred over an item with no correct answers, assuming the
     * interval criteria (difference with the prompt number) is satisfied.
     */
    def findPresentableQuizItem(qg: QuizGroup, params: NoParams): Option[QuizItem] =
      (for {
        (memLevel, levelIndex) <- qg.levels.zipWithIndex.reverse.tail.toStream
        quizItem <- produceQuizItemQuizGroupMemoryLevel.findPresentableQuizItem(memLevel,
            CurrentPromptNumber(qg)).toStream
      } yield quizItem).headOption

    def findAnyUnfinishedQuizItem(qg: QuizGroup, params: NoParams): Option[QuizItem] =
      (for {
        (memLevel, levelIndex) <- qg.levels.zipWithIndex.reverse.tail.toStream
        quizItem <- produceQuizItemQuizGroupMemoryLevel.findAnyUnfinishedQuizItem(
            memLevel, CurrentPromptNumber(qg))
      } yield quizItem).headOption
  }

  implicit object produceQuizItemQuizGroupMemoryLevel
      extends ProduceQuizItem[QuizGroupMemoryLevel, CurrentPromptNumber, QuizItem] {

    def findPresentableQuizItem(qgml: QuizGroupMemoryLevel, params: CurrentPromptNumber):
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