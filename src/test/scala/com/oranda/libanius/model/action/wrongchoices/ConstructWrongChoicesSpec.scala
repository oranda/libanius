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

import com.oranda.libanius.model.action.serialize.CustomFormatParserFast._
import fastparse.core.Parsed
import org.specs2.mutable.Specification
import com.oranda.libanius.dependencies.AppDependencyAccess
import com.oranda.libanius.model.action.wrongchoices.ConstructWrongChoices._
import com.oranda.libanius.model.action.wrongchoices.ConstructWrongChoicesForModelComponents._
import com.oranda.libanius.model.TestData._
import com.oranda.libanius.model.quizgroup.QuizGroupMemoryLevel
import com.oranda.libanius.model.quizitem.{QuizItem, TextValueOps}
import com.oranda.libanius.util.Util


class ConstructWrongChoicesSpec extends Specification with AppDependencyAccess {

  "the construct wrong choices functionality " should {

    "generate false answers similar to a correct answer" in {

      val falseAnswers = constructWrongChoicesSimilar(qgMemLevel,
          itemCorrect = QuizItem("entertain", "unterhalten"),
          correctResponses = List("unterhalten"),
          numWrongChoicesRequired = 5,
        similarityPredicate = TextValueOps.sameEnd)

      falseAnswers.contains("unterrichten") mustEqual true
    }

    "generate false answers similar to a correct answer, not including the correct answer" in {
      val quizItem1 = QuizItem("teach", "unterrichten")
      val quizItem2 = QuizItem("entertain", "unterhalten")
      val smallQuizGroupLevel = QuizGroupMemoryLevel(0, 0, List(quizItem1, quizItem2).toStream)

      val falseAnswers = constructWrongChoicesSimilar(smallQuizGroupLevel,
        itemCorrect = QuizItem("entertain", "unterhalten"),
        correctResponses = List("unterhalten"),
        numWrongChoicesRequired = 2,
        similarityPredicate = TextValueOps.sameEnd)

      falseAnswers.contains("unterhalten") mustEqual false
    }

    "construct wrong choices" in {
      val quizItemCorrect: QuizItem =
        qgWithHeader.quizGroup.quizItems.find(_.prompt.value == "entertain").get

      val (falseAnswers, timeTaken) = Util.stopwatch(ConstructWrongChoices.execute(
        qgWithHeader.quizGroup,
        quizItemCorrect,
        numWrongChoicesRequired = 2))

      falseAnswers.contains("unterbrochen") mustEqual true
      timeTaken must be lessThan 200
    }

    /**
     * This reproduces a (former) bug, where only one wrong choice was being produced.
     */

    "construct a full set of wrong choices" in {

      val demoGroupHeader = """#quizGroup type="WordMapping" promptType="German word" responseType="English word" isActive="true" currentPromptNumber="21""""
      val demoGroupText =
          s"""$demoGroupHeader
            |#quizGroupPartition numCorrectResponsesInARow="0" repetitionInterval="0"
            |entertain|unterhalten|
            |#quizGroupPartition numCorrectResponsesInARow="1" repetitionInterval="5"
            |against|wider|13;
            |#quizGroupPartition numCorrectResponsesInARow="2" repetitionInterval="15"
            |treaty|Vertrag|11,5;
            |contract|Vertrag|9,3;
            |en route|unterwegs|7,1;
            |""".stripMargin


      import com.oranda.libanius.model.action.serialize._
      import CustomFormat._
      import CustomFormatForModelComponents._

      val Parsed.Success(demoGroup, _) = quizGroupWithHeader.parse(demoGroupText)
      val quizItemCorrect: QuizItem = demoGroup.quizItems.find(_.prompt.value == "treaty").get

      val (falseAnswers, _) = Util.stopwatch(ConstructWrongChoices.execute(demoGroup,
        quizItemCorrect, numWrongChoicesRequired = 2))

      falseAnswers.size mustEqual 2
    }
  }
}
