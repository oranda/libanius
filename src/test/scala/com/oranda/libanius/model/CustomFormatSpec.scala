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

class CustomFormatSpec extends Specification with AppDependencyAccess {

  def paramsDefault = ParamsDefault(new StringBuilder)
  def paramsWithSeparator = ParamsWithSeparator(new StringBuilder, "|")
  def paramsWithSeparatorAndIndex = ParamsWithSeparatorAndIndex(new StringBuilder(), "|", 0)

  val quizItem = QuizItem(TextValue("solve"), TextValue("nachlösen"), userResponses)
  val quizItemCustomFormat = "solve|nachlösen|9,7;6"

  "the custom-format functionality " should {

    "serialize a word mapping value" in {
      val customFormat = to(wmv, paramsWithSeparator).toString
      customFormat mustEqual wmvCustomFormat
    }

    "serialize a word mapping value set" in {
      val customFormat = to(wmvs, paramsWithSeparator)
      customFormat.toString mustEqual wmvsCustomFormat
    }

    "serialize a quiz item" in {
      val customFormat = to(quizItem, paramsWithSeparator)
      customFormat.toString mustEqual quizItemCustomFormat
    }

    "serialize a quiz group memory level" in {
      val customFormat = to(qgMemLevelSimple, paramsWithSeparatorAndIndex)
      customFormat.toString mustEqual
        "#quizGroupPartition numCorrectResponsesInARow=\"0\" repetitionInterval=\"0\"\n" +
          qgMemLevelSimpleCustomFormat
    }

    "serialize a quiz group" in {
      val customFormat = to(qgWithHeader.quizGroup, paramsWithSeparator)
      customFormat.toString mustEqual qgCustomFormat
    }

    "serialize a quiz group with header" in {
      val customFormat = to(qgWithHeader, paramsDefault)
      customFormat.toString mustEqual qgwhCustomFormat
    }
  }

}
