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

package com.oranda.libanius.model.action.serialize

import org.specs2.mutable.Specification
import com.oranda.libanius.dependencies.AppDependencyAccess
import java.lang.StringBuilder

import com.oranda.libanius.model.action.serialize._
import CustomFormat._
import CustomFormatForModelComponents._
import com.oranda.libanius.model.quizitem.QuizItem
import com.oranda.libanius.model.wordmapping.{WordMappingValueSet, WordMappingValue}
import com.oranda.libanius.model.quizgroup.{QuizGroupHeader, QuizGroupWithHeader}
import com.oranda.libanius.model.TestData._

class CustomFormatSpec extends Specification with AppDependencyAccess {

  def paramsDefault = NoParams()
  def paramsWithSeparator = Separator("|")
  def paramsWithSeparatorAndIndex = SeparatorAndIndex("|", 0)

  val quizItem = QuizItem("solve", "nachlösen", userResponsesAll)
  val quizItemCustomFormat = "solve|nachlösen|9,7;6"

  def strBuilder = new StringBuilder

  "the custom-format functionality " should {

    "serialize a word mapping value" in {
      val customFormat = serialize(wmv, strBuilder, paramsWithSeparator).toString
      customFormat mustEqual wmvCustomFormat
    }

    "serialize a word mapping value set" in {
      val customFormat = serialize(wmvs, strBuilder, paramsWithSeparator)
      customFormat.toString mustEqual wmvsCustomFormat
    }

    "serialize a quiz item" in {
      val customFormat = serialize(quizItem, strBuilder, paramsWithSeparator)
      customFormat.toString mustEqual quizItemCustomFormat
    }

    "serialize a quiz group memory level" in {
      val customFormat = serialize(qgMemLevelSimple, strBuilder, paramsWithSeparatorAndIndex)
      val qgpHeader = "#quizGroupPartition numCorrectResponsesInARow=\"0\" repetitionInterval=\"0\"\n"
      customFormat.toString mustEqual qgpHeader + qgMemLevelSimpleCustomFormat
    }

    "serialize a quiz group" in {
      val customFormat = serialize(qgWithHeader.quizGroup, strBuilder, paramsWithSeparator)
      customFormat.toString mustEqual qgBodyCustomFormat
    }

    "serialize a quiz group with header" in {
      val customFormat = serialize(qgWithHeader, strBuilder, paramsDefault)
      customFormat.toString mustEqual qgwhCustomFormat
    }

    "deserialize a word mapping value" in {
      val wmv: WordMappingValue = deserialize[WordMappingValue, Separator](
          wmvCustomFormat, Separator("|"))
      wmv.value mustEqual "nachlösen"
      wmv.userAnswers.length mustEqual 3
      wmv.numCorrectAnswersInARow mustEqual 2
    }

    "deserialize a word mapping value set" in {
      val wmvs = deserialize[WordMappingValueSet, Separator](wmvsCustomFormat, Separator("|"))
      wmvs.containsValue("treaty")
      wmvs.size mustEqual 2
    }

    "deserialize a quiz item" in {
      val quizItemStr = "on|auf|"
      val quizItem = deserialize[QuizItem, Separator](quizItemStr, Separator("|"))
      val quizItemExpected = QuizItem("on", "auf")
      quizItem mustEqual quizItemExpected
    }

    "deserialize a quiz item with a special separator" in {
      val quizItemStr = "Given a String s = \"2.3\" convert it to a Double ||| s.toDouble"
      val quizItem = deserialize[QuizItem, Separator](quizItemStr, Separator("|||"))
      val quizItemExpected = QuizItem("Given a String s = \"2.3\" convert it to a Double",
          "s.toDouble")
      quizItem mustEqual quizItemExpected
    }

    "deserialize a quiz group memory level" in {
      import CustomFormat._
      import CustomFormatForModelComponents._

      val qgml = deserialize(qgMemLevelSimpleCustomFormat,
          SeparatorIndexAndRepetitionInterval("|", 0, 0))
      qgml.numQuizItems mustEqual 2
    }

    "deserialize a QuizGroup header" in {
      val qgh = deserialize[QuizGroupHeader, NoParams](qghCustomFormat, NoParams())
      qgh.promptType mustEqual "English word"
      qgh.responseType mustEqual "German word"
    }

    "deserialize a QuizGroupWithHeader" in {
      val qgwh = deserialize[QuizGroupWithHeader, Separator](qgwhCustomFormat, Separator("|"))
      qgwh.currentPromptNumber mustEqual 10
      qgwh.promptType mustEqual "English word"
      qgwh.responseType mustEqual "German word"

      qgwh.levels.head.numQuizItems mustEqual 8
      qgwh.levels(1).numQuizItems mustEqual 2
      qgwh.levels(2).numQuizItems mustEqual 2
    }
  }
}
