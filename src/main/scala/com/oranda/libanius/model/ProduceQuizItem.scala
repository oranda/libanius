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

package com.oranda.libanius.model

import com.oranda.libanius.model.quizitem.QuizItem
import com.oranda.libanius.model.quizgroup.{QuizGroupMemoryLevel, QuizGroup}

/**
 * Type class definition for finding quiz items in model entities.
 */
trait ProduceQuizItem[A <: ModelComponent, B <: Params] {
  def findPresentableQuizItem(component: A, params: B): Option[QuizItem]
  def findAnyUnfinishedQuizItem(component: A, params: B): Option[QuizItem]
}

trait Params {}
case class Empty() extends Params
case class CurrentPromptNumber(currentPromptNumber: Int) extends Params
object CurrentPromptNumber {
  def apply(qg: QuizGroup): CurrentPromptNumber = CurrentPromptNumber(qg.currentPromptNumber)
}

// provides external access to the typeclass, forwarding the call to the appropriate type
object ProduceQuizItem {

  def findPresentableQuizItem[A <: ModelComponent, B <: Params](component: A, params: B)
      (implicit pqi: ProduceQuizItem[A, B]): Option[QuizItem] =
    pqi.findPresentableQuizItem(component, params)

  def findAnyUnfinishedQuizItem[A <: ModelComponent, B <: Params](component: A, params: B)
      (implicit pqi: ProduceQuizItem[A, B]): Option[QuizItem] =
    pqi.findAnyUnfinishedQuizItem(component, params)
}

object ProduceQuizItemForModelComponents {

  implicit object produceQuizItemQuizGroup extends ProduceQuizItem[QuizGroup, Empty] {
    /*
     * The memLevels are searched in reverse order for presentable quiz items,
     * meaning that an item that has been answered correctly (once or more) will
     * be preferred over an item with no correct answers, assuming the
     * interval criteria (difference with the prompt number) is satisfied.
     */
    def findPresentableQuizItem(qg: QuizGroup, params: Empty): Option[QuizItem] =
      (for {
        (memLevel, levelIndex) <- qg.levels.zipWithIndex.reverse.tail.toStream
        quizItem <- produceQuizItemQuizGroupMemoryLevel.findPresentableQuizItem(memLevel,
            CurrentPromptNumber(qg)).toStream
      } yield quizItem).headOption


    def findAnyUnfinishedQuizItem(qg: QuizGroup, params: Empty): Option[QuizItem] =
      (for {
        (memLevel, levelIndex) <- qg.levels.zipWithIndex.reverse.tail.toStream
        quizItem <- produceQuizItemQuizGroupMemoryLevel.findAnyUnfinishedQuizItem(
            memLevel, CurrentPromptNumber(qg))
      } yield quizItem).headOption
  }

  implicit object produceQuizItemQuizGroupMemoryLevel
      extends ProduceQuizItem[QuizGroupMemoryLevel, CurrentPromptNumber] {

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