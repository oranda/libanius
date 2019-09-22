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

import com.oranda.libanius.dependencies.AppDependencyAccess
import com.oranda.libanius.model.TestData._
import com.oranda.libanius.model.action.serialize.CustomFormatParserFast.dequotedInt
import com.oranda.libanius.model.action.serialize.CustomFormatParserFast.quizGroupMemoryLevel
import com.oranda.libanius.model.action.serialize.CustomFormatParserFast.quizGroupMemoryLevelBody
import com.oranda.libanius.model.action.serialize.CustomFormatParserFast.quizGroupMemoryLevelHeader
import com.oranda.libanius.model.action.serialize.CustomFormatParserFast.userResponse
import com.oranda.libanius.model.action.serialize.CustomFormatParserFast.userResponses
import com.oranda.libanius.model.action.serialize.CustomFormatParserFast.userResponsesAll
import com.oranda.libanius.model.action.serialize.CustomFormatParserFast.wordMappingValue
import com.oranda.libanius.model.action.serialize.CustomFormatParserFast.wordMappingValueSet
import com.oranda.libanius.model.quizgroup.WordMapping
import com.oranda.libanius.model.quizitem.QuizItem
import com.oranda.libanius.model.{UserResponsesAll, TestData, UserResponse}
import fastparse.all._
import fastparse.core.Parsed
import org.specs2.mutable.Specification

import CustomFormatParserFast._

import scala.collection.immutable.List

class CustomFormatParserFastSpec extends Specification with AppDependencyAccess {

  "deserialize a user response" in {
    val Parsed.Success(ur, _) = userResponse.parse(TestData.userResponse)
    ur mustEqual Some(UserResponse(796))
  }

  "deserialize a list of responses" in {
    val Parsed.Success(urs, _) = userResponses.parse(TestData.userResponses)
    urs mustEqual List(UserResponse(9), UserResponse(7))
  }

  "deserialize an empty list of responses" in {
    val Parsed.Success(urs, _) = userResponses.parse("")
    urs mustEqual Nil
  }

  "deserialize all user responses" in {
    val Parsed.Success(ursa, _) = userResponsesAll.parse(TestData.userResponsesAllText)
    ursa mustEqual UserResponsesAll(List(UserResponse(9), UserResponse(7)), List(UserResponse(6)))
  }

  "deserialize all user responses where there are none" in {
    val Parsed.Success(ursa, _) = userResponsesAll.parse("")
    ursa mustEqual UserResponsesAll(Nil, Nil)
  }

  "deserialize all user responses -- optimized version" in {
    val Parsed.Success(ursa, _) = userResponsesAllFast.parse(TestData.userResponsesAllText)
    ursa mustEqual UserResponsesAll(List(UserResponse(9), UserResponse(7)), List(UserResponse(6)))
  }

  "deserialize all user responses where there are none -- optimized version" in {
    val Parsed.Success(ursa, _) = userResponsesAllFast.parse("")
    ursa mustEqual UserResponsesAll(Nil, Nil)
  }

  "deserialize a word mapping value" in {
    val Parsed.Success(wmv, _) = wordMappingValue(Separator("|")).parse(wmvCustomFormat)
    wmv.value mustEqual "nachlösen"
    wmv.userAnswers.length mustEqual 3
    wmv.numCorrectAnswersInARow mustEqual 2
  }

  "deserialize a word mapping value set" in {
    val Parsed.Success(wmvs, _) = wordMappingValueSet(Separator("|")).parse(wmvsCustomFormat)
    wmvs.containsValue("treaty")
    wmvs.size mustEqual 2
  }

  "deserialize a quiz item" in {
    val quizItemStr = "on|auf"
    implicit val sep = Separator("|")
    val Parsed.Success(qi, _) = CustomFormatParserFast.quizItem.parse(quizItemStr)
    val quizItemExpected = QuizItem("on", "auf")
    qi mustEqual quizItemExpected
  }

  "deserialize a quiz item with a special separator" in {
    val quizItemStr = "Type the bar character ||| | |||"
    implicit val sep = Separator("|||")
    val Parsed.Success(qi, _) = CustomFormatParserFast.quizItem.parse(quizItemStr)
    val quizItemExpected = QuizItem("Type the bar character", "|")
    qi mustEqual quizItemExpected
  }

  "deserialize a quoted String" in {
    val Parsed.Success(quotedStr, _) = dequotedString.parse("\"a\"")
    quotedStr mustEqual "a"
  }

  "deserialize a quoted int" in {
    val Parsed.Success(quotedInt, _) = dequotedInt.parse("\"5\"")
    quotedInt mustEqual 5
  }

  "deserialize a name-value-pair with an integer value" in {
    val input = "numCorrectResponsesInARow=\"5\""
    val name = "numCorrectResponsesInARow"
    val Parsed.Success(value, _) = CustomFormatParserFast.nvpInt(name).parse(input)
    value mustEqual 5
  }

  // Example input: #quizGroupPartition numCorrectResponsesInARow="0" repetitionInterval="0"
  "deserialize a quiz group memory level header" in {
    val Parsed.Success(value, _) = quizGroupMemoryLevelHeader.parse(qgMemLevelHeaderCustomFormat)
    value mustEqual (0, 0)
  }

  "deserialize a quiz group memory level body" in {
    implicit val sep = Separator("|")
    val Parsed.Success(body, _) = quizGroupMemoryLevelBody.parse(qgMemLevelSimpleCustomFormat)
    body.size mustEqual 2
  }

  "deserialize a quiz group memory level" in {
    implicit val sep = Separator("|")
    val Parsed.Success(qgml, _) = quizGroupMemoryLevel.parse(qgMemLevelWithHeaderCustomFormat)
    qgml.numQuizItems mustEqual 2
  }

  "deserialize quiz group user data" in {
    val Parsed.Success(qgud, _) = quizGroupUserData.parse(qgudCustomFormat)
    qgud.isActive mustEqual true
    qgud.currentPromptNumber mustEqual 10
  }

  "deserialize quiz group header with user data" in {
    val Parsed.Success((quizGroupHeader, userData), _) =
      quizGroupHeaderAndUserData.parse(qghCustomFormat)
    quizGroupHeader.quizGroupType mustEqual WordMapping
    quizGroupHeader.promptType mustEqual "English word"
    quizGroupHeader.responseType mustEqual "German word"
    quizGroupHeader.mainSeparator mustEqual Separator("|")
    userData.isActive mustEqual true
    userData.currentPromptNumber mustEqual 10
  }

  "deserialize quiz group header with user data and no separator" in {
    val Parsed.Success((quizGroupHeader, userData), _) =
      quizGroupHeaderAndUserData.parse(qghCustomFormatNoSeparator)
    quizGroupHeader.quizGroupType mustEqual WordMapping
    quizGroupHeader.promptType mustEqual "English word"
    quizGroupHeader.responseType mustEqual "German word"
    quizGroupHeader.mainSeparator mustEqual Separator("|")
    userData.isActive mustEqual true
    userData.currentPromptNumber mustEqual 10
  }

  "deserialize quiz group header with user data and no useMultipleChoiceUntil field" in {
    val Parsed.Success((quizGroupHeader, userData), _) =
      quizGroupHeaderAndUserData.parse(qghCustomFormatNoUseMultipleChoiceUntil)
    quizGroupHeader.quizGroupType mustEqual WordMapping
    quizGroupHeader.promptType mustEqual "English word"
    quizGroupHeader.responseType mustEqual "German word"
    quizGroupHeader.mainSeparator mustEqual Separator("|")
    userData.isActive mustEqual true
    userData.currentPromptNumber mustEqual 10
  }

  "deserialize a QuizGroup" in {
    val Parsed.Success(qgwh, _) = quizGroupBody(Separator("|")).parse(qgBodyCustomFormat)

    qgwh(0).numQuizItems mustEqual 8
    qgwh(1).numQuizItems mustEqual 2
    qgwh(2).numQuizItems mustEqual 2
  }

  "deserialize a QuizGroupWithHeader" in {
    val Parsed.Success(qgwh, _) = quizGroupWithHeader.parse(qgwhCustomFormat)
    qgwh.currentPromptNumber mustEqual 10
    qgwh.promptType mustEqual "English word"
    qgwh.responseType mustEqual "German word"

    qgwh.levels.head.numQuizItems mustEqual 8
    qgwh.levels(1).numQuizItems mustEqual 2
    qgwh.levels(2).numQuizItems mustEqual 2
  }

}
