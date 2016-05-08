/*
 * Libanius
 * Copyright (C) 2012-2016 James McCabe <james@oranda.com>
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
import com.oranda.libanius.model._
import com.oranda.libanius.model.action.serialize.CustomFormatParser._
import com.oranda.libanius.model.quizgroup.{QuizGroupMemoryLevel, QuizGroupUserData, QuizGroupHeader, WordMapping}
import com.oranda.libanius.model.quizitem.QuizItem
import com.oranda.libanius.model.wordmapping.WordMappingValue
import org.specs2.mutable.Specification

import scala.Error
import scala.collection.immutable.{List, Stream}
import scala.util.parsing.input.{Reader, CharSequenceReader}

class CustomFormatParserSpec extends Specification with AppDependencyAccess {

  val parsers = CustomFormatParser
  import parsers._

  def toReader(str: String) = new CharSequenceReader(str)

  "deserialize a user response" in {
    val ur: UserResponse = parseUserResponse(toReader(TestData.userResponse))
    ur mustEqual UserResponse(796)
  }

  "deserialize a list of responses" in {
    val urs: List[UserResponse] = parseUserResponses(toReader(TestData.userResponses))
    urs mustEqual List(UserResponse(9), UserResponse(7))
  }

  "deserialize all user responses" in {
    val ursa = parseUserResponsesAll(toReader(TestData.userResponsesAllText))
    ursa mustEqual UserResponsesAll(List(UserResponse(9), UserResponse(7)), List(UserResponse(6)))
  }

  "deserialize all user responses where there are none" in {
    val ursa = parseUserResponsesAll(toReader(""))
    ursa mustEqual UserResponsesAll(Nil, Nil)
  }

  "deserialize a word mapping value" in {
    val wmv: WordMappingValue = parseWordMappingValue(toReader(wmvCustomFormat))(Separator("|"))
    wmv.value mustEqual "nachlösen"
    wmv.userAnswers.length mustEqual 3
    wmv.numCorrectAnswersInARow mustEqual 2
  }

  "deserialize a word mapping value set" in {
    val wmvs = parseWordMappingValueSet(toReader(wmvsCustomFormat))(Separator("|"))
    wmvs.containsValue("treaty")
    wmvs.size mustEqual 2
  }

  "deserialize a quiz item" in {
    val quizItemStr = "on|auf"
    val qi = parseQuizItem(toReader(quizItemStr))(Separator("|"))
    val quizItemExpected = QuizItem("on", "auf")
    qi mustEqual quizItemExpected
  }

  "deserialize a quiz item with a special separator" in {
    val quizItemStr = "Given a String s = \"2.3\" convert it to a Double ||| s.toDouble|||"
    val qi = parseQuizItem(toReader(quizItemStr))(Separator("|||"))
    val quizItemExpected = QuizItem(
      "Given a String s = \"2.3\" convert it to a Double",
      "s.toDouble")
    qi mustEqual quizItemExpected
  }

  "deserialize a quoted int" in {
    val quotedInt = parseDequotedInt("\"5\"")
    quotedInt mustEqual 5
  }

  "deserialize a name-value-pair with an integer value" in {
    val nvpInt = parseNvpInt("numCorrectResponsesInARow=\"5\"", "numCorrectResponsesInARow")
    nvpInt mustEqual 5
  }


  // Example: #quizGroupPartition numCorrectResponsesInARow="0" repetitionInterval="0"
  "deserialize a quiz group memory level header" in {
    val nvpInt = parseQuizGroupMemoryLevelHeader(toReader(qgMemLevelHeaderCustomFormat))
    nvpInt mustEqual (0, 0)
  }

  "deserialize a quiz group memory level body" in {
    val qgmlb = parseQuizGroupMemoryLevelBody(toReader(qgMemLevelSimpleCustomFormat),
      SeparatorIndexAndRepetitionInterval(Separator("|"), 0, 0))
    qgmlb.size mustEqual 2
  }


  "deserialize a quiz group memory level" in {
    val qgml = parseQuizGroupMemoryLevel(toReader(qgMemLevelWithHeaderCustomFormat),
      SeparatorIndexAndRepetitionInterval(Separator("|"), 0, 0))
    qgml.numQuizItems mustEqual 2
  }


  "deserialize quiz group user data" in {
    val qgud = parseQuizGroupUserData(toReader(qgudCustomFormat))

    qgud.isActive mustEqual true
    qgud.currentPromptNumber mustEqual 10
  }

  "deserialize quiz group header with user data" in {
    val qghAndUd = parseQuizGroupHeaderAndUserData(toReader(qghCustomFormat))
    qghAndUd._1.quizGroupType mustEqual WordMapping
    qghAndUd._1.promptType mustEqual "English word"
    qghAndUd._1.responseType mustEqual "German word"
    qghAndUd._1.mainSeparator mustEqual Separator("|")
    qghAndUd._2.isActive mustEqual true
    qghAndUd._2.currentPromptNumber mustEqual 10
  }

  "deserialize quiz group header with user data and no separator" in {
    val qghAndUd = parseQuizGroupHeaderAndUserData(toReader(qghCustomFormatNoSeparator))
    qghAndUd._1.quizGroupType mustEqual WordMapping
    qghAndUd._1.promptType mustEqual "English word"
    qghAndUd._1.responseType mustEqual "German word"
    qghAndUd._1.mainSeparator mustEqual Separator("|")
    qghAndUd._2.isActive mustEqual true
    qghAndUd._2.currentPromptNumber mustEqual 10
  }

  "deserialize quiz group header with user data and no useMultipleChoiceUntil field" in {
    val qghAndUd = parseQuizGroupHeaderAndUserData(toReader(qghCustomFormatNoUseMultipleChoiceUntil))
    qghAndUd._1.quizGroupType mustEqual WordMapping
    qghAndUd._1.promptType mustEqual "English word"
    qghAndUd._1.responseType mustEqual "German word"
    qghAndUd._1.mainSeparator mustEqual Separator("|")
    qghAndUd._2.isActive mustEqual true
    qghAndUd._2.currentPromptNumber mustEqual 10
  }

  "deserialize a QuizGroup" in {
    val qgwh = parseQuizGroupBody(qgBodyCustomFormat)(Separator("|"))

    qgwh(0).numQuizItems mustEqual 8
    qgwh(1).numQuizItems mustEqual 2
    qgwh(2).numQuizItems mustEqual 2
  }


  "deserialize a QuizGroupWithHeader" in {
    val qgwh = parseQuizGroupWithHeader(toReader(qgwhCustomFormat))
    qgwh.currentPromptNumber mustEqual 10
    qgwh.promptType mustEqual "English word"
    qgwh.responseType mustEqual "German word"

    qgwh.levels(0).numQuizItems mustEqual 8
    qgwh.levels(1).numQuizItems mustEqual 2
    qgwh.levels(2).numQuizItems mustEqual 2
  }

  def parseQuizGroupHeaderAndUserData(qghudText: Reader[Char]): (QuizGroupHeader, QuizGroupUserData) =
    parse(quizGroupHeaderAndUserData, qghudText) match {
      case Success(matched, _) => matched
      case Failure(msg, _) => println("FAILURE: " + msg); (QuizGroupHeader(""), QuizGroupUserData())
      case Error(msg, _) => println("ERROR: " + msg); (QuizGroupHeader(""), QuizGroupUserData())
    }

  def parseQuizGroupBody(qgText: String)(implicit sep: Separator): Map[Int, QuizGroupMemoryLevel] =
    parse(quizGroupBody, qgText) match {
      case Success(matched, _) => matched
      case Failure(msg, _) => println("FAILURE: " + msg); Map.empty
      case Error(msg, _) => println("ERROR: " + msg); Map.empty
    }

  def parseQuizGroupMemoryLevel(qgmlText: Reader[Char], sep: SeparatorIndexAndRepetitionInterval):
      QuizGroupMemoryLevel = {
    val qgml = QuizGroupMemoryLevel(correctResponsesInARow = 0, repetitionInterval = 0,
      quizItemStream = Stream.empty, totalResponses = 0, numCorrectResponses = 0)
    parse(quizGroupMemoryLevel(sep.separator), qgmlText) match {
      case Success(matched, _) => matched
      case Failure(msg, _) => println("FAILURE: " + msg); qgml
      case Error(msg, _) => println("ERROR: " + msg); qgml
    }
  }

  def parseQuizGroupMemoryLevelBody(qgmlText: Reader[Char], sep: SeparatorIndexAndRepetitionInterval):
      List[QuizItem] =
    parse(quizGroupMemoryLevelBody(sep.separator), qgmlText) match {
      case Success(matched, _) => matched
      case Failure(msg, _) => println("FAILURE parseQuizGroupMemoryLevelBody: " + msg); Nil
      case Error(msg, _) => println("ERROR: " + msg); Nil
    }

  def parseQuizGroupMemoryLevelHeader(qText: Reader[Char])(implicit sep: Separator = Separator("|")): (Int, Int) =
    parse(quizGroupMemoryLevelHeader, qText) match {
      case Success(matched, _) => matched
      case Failure(msg, _) => println("FAILURE: " + msg); (-1, -1)
      case Error(msg, _) => println("ERROR: " + msg); (-1, -1)
    }

  def parseDequotedInt(text: String): Int =
    parse(dequotedInt, text) match {
      case Success(matched, _) => matched
      case Failure(msg, _) => println("FAILURE: " + msg); -1
      case Error(msg, _) => println("ERROR: " + msg); -1
    }

  def parseQuizItem(qgText: Reader[Char])(implicit sep: Separator): QuizItem =
    parse(quizItem, qgText) match {
      case Success(matched, _) => matched
      case Failure(msg, _) => println("FAILURE: " + msg); QuizItem("", "")
      case Error(msg, _) => println("ERROR: " + msg); QuizItem("", "")
    }

  def parseWordMappingValue(urText: Reader[Char])(implicit sep: Separator): WordMappingValue =
    parse(wordMappingValue, urText) match {
      case Success(matched, _) => matched
      case Failure(msg, _) => println("FAILURE: " + msg); WordMappingValue(QuizItem("", ""))
      case Error(msg, _) => println("ERROR: " + msg); WordMappingValue(QuizItem("", ""))
    }

  def parseUserResponsesAll(urText: Reader[Char]): UserResponsesAll =
    parse(userResponsesAll, urText) match {
      case Success(matched, _) => matched
      case Failure(msg, _) => println("FAILURE: " + msg); UserResponsesAll()
      case Error(msg, _) => println("ERROR: " + msg); UserResponsesAll()
    }

  def parseUserResponses(ursText: Reader[Char]): List[UserResponse] =
    parse(userResponses, ursText) match {
      case Success(matched, _) => matched
      case Failure(msg, _) => println("FAILURE: " + msg); Nil
      case Error(msg, _) => println("ERROR: " + msg); Nil
    }

  def parseUserResponse(urText: Reader[Char]): UserResponse =
    parse(userResponse, urText) match {
      case Success(matched, _) => matched
      case Failure(msg, _) => println("FAILURE: " + msg); UserResponse(-1)
      case Error(msg, _) => println("ERROR: " + msg); UserResponse(-1)
    }
}
