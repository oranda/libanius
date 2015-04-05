/*
 * Libanius
 * Copyright (C) 2012-2015 James McCabe <james@oranda.com>
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

import org.specs2.mutable.Specification
import com.oranda.libanius.dependencies.AppDependencyAccess
import com.oranda.libanius.model.action.wrongchoices.ConstructWrongChoices._
import com.oranda.libanius.model.action.wrongchoices.ConstructWrongChoicesForModelComponents._
import com.oranda.libanius.model.TestData._
import com.oranda.libanius.model.quizitem.{TextValue, QuizItem}
import com.oranda.libanius.util.Util
import com.oranda.libanius.model.quizgroup.QuizGroup


class ConstructWrongChoicesSpec extends Specification with AppDependencyAccess {

  "the construct wrong choices functionality " should {
    "generate false answers similar to a correct answer" in {

      val falseAnswers = constructWrongChoicesSimilar(qgMemLevel,
          itemCorrect = QuizItem("entertain", "unterhalten"),
          correctResponses = List("unterhalten"),
          numWrongChoicesRequired = 5,
          similarityPredicate = TextValue.sameEnd)

      falseAnswers.contains("unterrichten") mustEqual true
    }

    "construct wrong choices" in {
      val quizItemCorrect: QuizItem = qgWithHeader.quizGroup.quizItems.find(
        _.prompt.value == "entertain").get

      val (falseAnswers, timeTaken) = Util.stopwatch(ConstructWrongChoices.execute(
          qgWithHeader.quizGroup, quizItemCorrect, numWrongChoicesRequired = 2))

      falseAnswers.contains("unterbrochen") mustEqual true
      timeTaken must be lessThan 100
    }

    /**
     * This reproduces a (former) bug, where only one wrong choice was being produced.
     */
    "construct a full set of wrong choices" in {

      val demoGroupHeader = "#quizGroup type=\"WordMapping\" promptType=\"German word\" responseType=\"English word\" currentPromptNumber=\"21\" isActive=\"true\"\n"
      val demoGroupText = demoGroupHeader +
          "#quizGroupPartition numCorrectResponsesInARow=\"0\" repetitionInterval=\"0\"\n" +
          "entertain|unterhalten'\n" +
          "#quizGroupPartition numCorrectResponsesInARow=\"1\" repetitionInterval=\"5\"\n" +
          "against|wider'13\n" +
          "#quizGroupPartition numCorrectResponsesInARow=\"2\" repetitionInterval=\"15\"\n" +
          "treaty|Vertrag'11,5\n" +
          "contract|Vertrag'9,3\n" +
          "en route|unterwegs'7,1\n"

      import com.oranda.libanius.model.action.serialize._
      import CustomFormat._
      import CustomFormatForModelComponents._

      val demoGroup: QuizGroup = deserialize[QuizGroup, Separator](demoGroupText, Separator("|"))
      val quizItemCorrect: QuizItem = demoGroup.quizItems.find(_.prompt.value == "treaty").get

      val (falseAnswers, _) = Util.stopwatch(ConstructWrongChoices.execute(demoGroup,
        quizItemCorrect, numWrongChoicesRequired = 2))

      falseAnswers.size mustEqual 2
    }
  }
}
