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

import org.specs2.mutable.Specification
import com.oranda.libanius.dependencies.AppDependencyAccess
import TestData._
import java.lang.StringBuilder

import CustomFormat._
import CustomFormatForModelComponents._
import com.oranda.libanius.model.quizitem.{TextValue, QuizItem}
import com.oranda.libanius.model.quizgroup.QuizGroup
import com.oranda.libanius.util.Util

class CustomFormatSpec extends Specification with AppDependencyAccess {

  def paramsDefault = ParamsDefault()
  def paramsWithSeparator = ParamsWithSeparator("|")
  def paramsWithSeparatorAndIndex = ParamsWithSeparatorAndIndex("|", 0)

  val quizItem = QuizItem(TextValue("solve"), TextValue("nachlösen"), userResponses)
  val quizItemCustomFormat = "solve|nachlösen|9,7;6"

  "the custom-format functionality " should {

    "serialize a word mapping value" in {
      val customFormat = to(wmv, new StringBuilder, paramsWithSeparator).toString
      customFormat mustEqual wmvCustomFormat
    }

    "serialize a word mapping value set" in {
      val customFormat = to(wmvs, new StringBuilder, paramsWithSeparator)
      customFormat.toString mustEqual wmvsCustomFormat
    }

    "serialize a quiz item" in {
      val customFormat = to(quizItem, new StringBuilder, paramsWithSeparator)
      customFormat.toString mustEqual quizItemCustomFormat
    }

    "serialize a quiz group memory level" in {
      val customFormat = to(qgMemLevelSimple, new StringBuilder, paramsWithSeparatorAndIndex)
      customFormat.toString mustEqual
        "#quizGroupPartition numCorrectResponsesInARow=\"0\" repetitionInterval=\"0\"\n" +
            qgMemLevelSimpleCustomFormat
    }

    "serialize a quiz group" in {
      val customFormat = to(qgWithHeader.quizGroup, new StringBuilder, paramsWithSeparator)
      customFormat.toString mustEqual qgCustomFormat
    }

    "serialize a quiz group with header" in {
      val customFormat = to(qgWithHeader, new StringBuilder, paramsDefault)
      customFormat.toString mustEqual qgwhCustomFormat
    }

    "deserialize a word mapping value" in {
      wmv.value mustEqual "nachlösen"
      wmv.userAnswers.length mustEqual 3
      wmv.numCorrectAnswersInARow mustEqual 2
    }

    "deserialize a word mapping value set" in {
      wmvs.containsValue("treaty")
      wmvs.size mustEqual 2
    }

    "deserialize a quiz item" in {
      val quizItemStr = "on|auf|"
      val quizItem = customFormatQuizItem.from(quizItemStr, FromParamsWithSeparator("|"))
      val quizItemExpected = QuizItem("on", "auf")
      quizItem mustEqual quizItemExpected
    }

    "deserialize a quiz item with a special separator" in {
      val quizItemStr = "Given a String s = \"2.3\" convert it to a Double ||| s.toDouble"
      val quizItem = customFormatQuizItem.from(quizItemStr, FromParamsWithSeparator("|||"))
      val quizItemExpected = QuizItem("Given a String s = \"2.3\" convert it to a Double",
        "s.toDouble")
      quizItem mustEqual quizItemExpected
    }

    "deserialize a quiz group memory level" in {
      import CustomFormat._
      import CustomFormatForModelComponents._

      val qgml = from(qgMemLevelSimpleCustomFormat,
          FromParamsWithSeparatorAndRepetitionInterval("|", 0))
      qgml.numQuizItems mustEqual 2
    }


    "deserialize a QuizGroupWithHeader" in {
      qgWithHeader.currentPromptNumber mustEqual 10
      qgWithHeader.promptType mustEqual "English word"
      qgWithHeader.responseType mustEqual "German word"

      qgWithHeader.levels(0).numQuizItems mustEqual 8
      qgWithHeader.levels(1).numQuizItems mustEqual 2
      qgWithHeader.levels(2).numQuizItems mustEqual 2
    }

    "deserialize a quiz" in {
      quiz.numActiveGroups mustEqual 2
      quiz.numQuizItems mustEqual 16
    }
  }
}
