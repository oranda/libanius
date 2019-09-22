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

package com.oranda.libanius.model.action.wrongchoices

import com.oranda.libanius.model.quizitem.TextValueOps.TextValue
import com.oranda.libanius.model.{ModelComponent}
import com.oranda.libanius.model.quizitem.{TextValueOps, QuizItem}
import com.oranda.libanius.dependencies.AppDependencyAccess
import com.oranda.libanius.model.quizgroup.{QuizGroupMemoryLevel, QuizGroup}
import scala.collection.immutable.{List, HashSet}
import scala.util.Random
import com.oranda.libanius.util.Util

/**
 * Type class definition for finding quiz items in model entities.
 */
trait ConstructWrongChoices[A <: ModelComponent] {
  def constructWrongChoicesSimilar(
      component: A,
      itemCorrect: QuizItem,
      numWrongChoicesRequired: Int,
      correctResponses: List[String],
      similarityPredicate: (TextValue, TextValue) => Int => Boolean): List[String]

  def constructWrongChoicesRandom(
      component: A,
      itemCorrect: QuizItem,
      numWrongChoicesRequired: Int,
      correctResponses: List[String]): List[String]
}

// provides external access to the typeclass
object ConstructWrongChoices extends AppDependencyAccess {

  def constructWrongChoicesSimilar[A <: ModelComponent](
      component: A,
      itemCorrect: QuizItem,
      numWrongChoicesRequired: Int,
      correctResponses: List[String],
      similarityPredicate: (TextValue, TextValue) => Int => Boolean)
      (implicit cwc: ConstructWrongChoices[A]): List[String] =
    cwc.constructWrongChoicesSimilar(component, itemCorrect, numWrongChoicesRequired,
        correctResponses, similarityPredicate)

  def constructWrongChoicesRandom[A <: ModelComponent](
      component: A,
      itemCorrect: QuizItem,
      numWrongChoicesRequired: Int,
      correctResponses: List[String])
      (implicit cwc: ConstructWrongChoices[A]): List[String] =
    cwc.constructWrongChoicesRandom(component, itemCorrect, numWrongChoicesRequired,
        correctResponses)


  protected[model] def execute(
      quizGroup: QuizGroup,
      itemCorrect: QuizItem,
      numWrongChoicesRequired: Int = 2): List[String] = {

    val correctResponses = Util.stopwatch(
        quizGroup.findResponsesFor(itemCorrect.prompt.value),
        "findResponsesFor")
    val numCorrectResponsesSoFar = itemCorrect.userResponses.numCorrectResponsesInARow

    val similarityPredicate =
      if (numCorrectResponsesSoFar % 2 == 1) TextValueOps.sameStart else TextValueOps.sameEnd

    implicit val cwc = ConstructWrongChoicesForModelComponents.cwcQuizGroup
    val badChoicesSimilar = constructWrongChoicesSimilar(quizGroup, itemCorrect,
        numWrongChoicesRequired, correctResponses, similarityPredicate)
    val badChoicesRandom = constructWrongChoicesRandom(quizGroup, itemCorrect,
        numWrongChoicesRequired, correctResponses)
    val badChoicesDummy = constructWrongChoicesDummy(numWrongChoicesRequired)
    val falseResponses: List[String] = badChoicesSimilar ++ badChoicesRandom ++ badChoicesDummy

    falseResponses.distinct.take(numWrongChoicesRequired)
  }

  protected[model] def constructWrongChoicesDummy(numWrongChoicesRequired: Int):
      List[String] = {
    val characters = "abcdefghijklmnopqrstuvwxyz0123456789".toCharArray
    if (numWrongChoicesRequired > characters.length) {
      l.logError("Too many dummy answers requested.")
      Nil
    } else
      characters.map(_.toString).take(numWrongChoicesRequired).toList
  }
}

object ConstructWrongChoicesForModelComponents extends AppDependencyAccess {

  implicit object cwcQuizGroup extends ConstructWrongChoices[QuizGroup] {
    /*
     * If the user has already been having success with this item, first try to
     * find responses that look similar to the correct one.
     */
    def constructWrongChoicesSimilar(
        quizGroup: QuizGroup,
        itemCorrect: QuizItem,
        numWrongChoicesRequired: Int,
        correctResponses: List[String],
        similarityPredicate: (TextValue, TextValue) => Int => Boolean): List[String] =
      if (itemCorrect.numCorrectResponsesInARow == 0) Nil
      else quizGroup.levels.flatMap(cwcQuizGroupMemoryLevel.constructWrongChoicesSimilar(
        _,
        itemCorrect,
        numWrongChoicesRequired,
        correctResponses,
        similarityPredicate))

    def constructWrongChoicesRandom(
        quizGroup: QuizGroup,
        itemCorrect: QuizItem,
        numWrongChoicesRequired: Int,
        correctResponses: List[String]): List[String] =
      quizGroup.levels.filterNot(_.isEmpty).flatMap(level =>
        cwcQuizGroupMemoryLevel.constructWrongChoicesRandom(
          level,
          itemCorrect,
          numWrongChoicesRequired,
          correctResponses))
  }

  implicit object cwcQuizGroupMemoryLevel extends ConstructWrongChoices[QuizGroupMemoryLevel] {

    def constructWrongChoicesSimilar(
        qgml: QuizGroupMemoryLevel,
        itemCorrect: QuizItem,
        numWrongResponsesRequired: Int,
        correctResponses: List[String],
        similarityPredicate: (TextValue, TextValue) => Int => Boolean): List[String] = {

      var similarWords = new HashSet[String]
      var numValueSetsSearched = 0
      val numSimilarLettersRequired = 2

      val correctValue = itemCorrect.correctResponse

      qgml.quizItems.iterator.takeWhile(_ => similarWords.size < numWrongResponsesRequired).
        foreach(quizItem => {
          numValueSetsSearched = numValueSetsSearched + 1
          // Avoid selecting values belonging to the "correct" correctResponse set
          if (!correctResponses.contains(quizItem.correctResponse.toString)) {
            val correctResponse = quizItem.correctResponse
            val areSimilar =
              similarityPredicate(correctResponse, correctValue)(numSimilarLettersRequired)
            if (similarWords.size < numWrongResponsesRequired && areSimilar)
              similarWords += quizItem.correctResponse.value
          }
      })
      similarWords.toList
    }

    def constructWrongChoicesRandom(
        qgml: QuizGroupMemoryLevel,
        itemCorrect: QuizItem,
        numWrongChoicesRequired: Int,
        correctResponses: List[String]): List[String] = {

      def randomFalseWordValue(sliceIndex: Int): Option[String] = {
        val sliceSize = (qgml.numQuizItems / numWrongChoicesRequired)
        val sliceStartIndex = sliceIndex * sliceSize
        val randomOffset = Random.nextInt(math.max(sliceSize, 1))
        val randomIndex = math.min(sliceStartIndex + randomOffset, qgml.numQuizItems - 1)
        val randomItem = Option(qgml.quizItems(randomIndex).correctResponse.value)
        randomItem.filter(!correctResponses.contains(_))
      }

      (0 until numWrongChoicesRequired)
        .map(sliceIndex => randomFalseWordValue(sliceIndex)).flatten.toList
    }
  }
}
