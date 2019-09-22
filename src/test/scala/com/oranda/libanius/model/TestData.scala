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

package com.oranda.libanius.model

import com.oranda.libanius.model.quizitem.QuizItem
import com.oranda.libanius.model.wordmapping.{WordMappingValue, WordMappingValueSet, Dictionary}
import com.oranda.libanius.model.quizgroup._

object TestData {

  val userResponse = "796"
  val userResponses = "9,7"
  val userResponsesAllText = "9,7;6"

  // word-mapping value
  val wmvCustomFormat = "nachlösen|9,7;6"
  val wmv: WordMappingValue = WordMappingValue("nachlösen",
      List(UserResponse(9), UserResponse(7)),
      List(UserResponse(6)))

  // word-mapping value set
  val wmvsCustomFormat = "contract|698,696;697/treaty|796;798"
  val wmvs: WordMappingValueSet = WordMappingValueSet(
    List(
      WordMappingValue("contract",
      List(
        UserResponse(698), UserResponse(696)), List(UserResponse(697))),
        WordMappingValue("treaty",
        List(UserResponse(796)), List(UserResponse(798)))
  ))

  // QuizItem data
  val correctAnswersInARow = List(UserResponse(9), UserResponse(7))
  val incorrectAnswers = List(UserResponse(6))
  val userResponsesAll = UserResponsesAll(correctAnswersInARow, incorrectAnswers)
  val quizItem = QuizItem("solve", "nachlösen", userResponsesAll)

  // Memory level
  /*
   * Construct a quiz group partition.
   */
  def makeQgMemLevel: QuizGroupMemoryLevel =
    QuizGroupMemoryLevel(
      0,
      0,
      List(
        QuizItem("against", "wider"),
        QuizItem("entertain", "unterhalten"),
        QuizItem("teach", "unterrichten"),
        QuizItem("winner", "Siegerin"),
        QuizItem("en route", "unterwegs"),
        QuizItem("full", "satt"),
        QuizItem("full", "voll"),
        QuizItem("interrupted", "unterbrochen"),
        QuizItem("contract", "Vertrag"),
        QuizItem("rides", "reitet"),
        QuizItem("on", "auf"),
        QuizItem("sweeps", "streicht")).toStream)

  def makeQgMemLevelSimple: QuizGroupMemoryLevel =
    QuizGroupMemoryLevel(
      0,
      0,
      List(QuizItem("against", "wider"), QuizItem("entertain", "unterhalten")).toStream)


  // defaults for read-only
  val qgMemLevel = makeQgMemLevel
  val qgMemLevelSimple = makeQgMemLevelSimple

  val eol = sys.props("line.separator")
  val qgMemLevelSimpleCustomFormat =
    """against|wider|
      |entertain|unterhalten|
      |""".stripMargin

  val qgMemLevelHeaderCustomFormat =
    """#quizGroupPartition numCorrectResponsesInARow="0" repetitionInterval="0""""

  val qgMemLevelWithHeaderCustomFormat =
    s"""$qgMemLevelHeaderCustomFormat\n$qgMemLevelSimpleCustomFormat"""

  val qgBodyCustomFormat =
      s"""$qgMemLevelHeaderCustomFormat
        |en route|unterwegs|
        |full|satt|
        |full|voll|
        |interrupted|unterbrochen|
        |contract|Vertrag|
        |rides|reitet|
        |on|auf|
        |sweeps|streicht|
        |#quizGroupPartition numCorrectResponsesInARow="1" repetitionInterval="5"
        |entertain|unterhalten|8;2
        |winner|Siegerin|5;0
        |#quizGroupPartition numCorrectResponsesInARow="2" repetitionInterval="15"
        |against|wider|9,7;6
        |teach|unterrichten|4,3;1
        |#quizGroupPartition numCorrectResponsesInARow="3" repetitionInterval="15"
        |#quizGroupPartition numCorrectResponsesInARow="4" repetitionInterval="60"
        |#quizGroupPartition numCorrectResponsesInARow="5" repetitionInterval="600"
        |#quizGroupPartition numCorrectResponsesInARow="6" repetitionInterval="0"
        |""".stripMargin

  val qgudCustomFormat = """isActive="true" currentPromptNumber="10""""
  val qghCustomFormat =
    """#quizGroup type="WordMapping" promptType="English word" responseType="German word" mainSeparator="|" useMultipleChoiceUntil="4" isActive="true" currentPromptNumber="10"""" + "\n"
  val qghCustomFormatNoSeparator =
    """#quizGroup type="WordMapping" promptType="English word" responseType="German word" useMultipleChoiceUntil="4" isActive="true" currentPromptNumber="10"""" + "\n"
  val qghCustomFormatNoUseMultipleChoiceUntil =
    """#quizGroup type="WordMapping" promptType="English word" responseType="German word" isActive="true" currentPromptNumber="10"""" + "\n"

  val qgwhCustomFormat = qghCustomFormat + qgBodyCustomFormat

  def makeQgMemLevel0: QuizGroupMemoryLevel =
    QuizGroupMemoryLevel(
      0,
      0,
      List(
        QuizItem("en route", "unterwegs"),
        QuizItem("full", "satt"),
        QuizItem("full", "voll"),
        QuizItem("interrupted", "unterbrochen"),
        QuizItem("contract", "Vertrag"),
        QuizItem("rides", "reitet"),
        QuizItem("on", "auf"),
        QuizItem("sweeps", "streicht")).toStream)

  def makeQgMemLevel1: QuizGroupMemoryLevel =
    QuizGroupMemoryLevel(
      1,
      5,
      List(
        QuizItem("entertain", "unterhalten", List(8), List(2)),
        QuizItem("winner", "Siegerin", List(5), List(0))).toStream)

  def makeQgMemLevel2: QuizGroupMemoryLevel =
    QuizGroupMemoryLevel(
      2,
      15,
      List(
        QuizItem("against", "wider", List(9, 7), List(6)),
        QuizItem("teach", "unterrichten", List(4, 3), List(1))).toStream)

  def makeQuizGroup =
    QuizGroup(
      Map(0 -> makeQgMemLevel0, 1-> makeQgMemLevel1, 2-> makeQgMemLevel2),
      QuizGroupUserData(isActive = true, 10),
      new Dictionary())

  val quizGroup = makeQuizGroup

  def makeSimpleQuizGroup =
    QuizGroup(Map(0 -> makeQgMemLevel0), QuizGroupUserData(isActive = true, 0))
  val quizGroupSimple = makeSimpleQuizGroup

  def makeQgwh(quizGroup: QuizGroup): QuizGroupWithHeader = {
    val header = QuizGroupHeader(WordMapping, "English word", "German word", "|", 4)
    QuizGroupWithHeader(header, quizGroup)
  }
  val qgwh: QuizGroupWithHeader = makeQgwh(quizGroup)

  def makeQgWithHeader: QuizGroupWithHeader = makeQgwh(makeQuizGroup)
  def makeSimpleQgWithHeader: QuizGroupWithHeader = makeQgwh(makeSimpleQuizGroup)

  val qgwhSimple: QuizGroupWithHeader = makeSimpleQgWithHeader

  // defaults for read-only
  val qgWithHeader = makeQgWithHeader

  val quizData = List(
    """#quizGroup type="WordMapping" promptType="English word" responseType="German word" isActive="true" currentPromptNumber="0"
      |#quizGroupPartition numCorrectResponsesInARow="0" repetitionInterval="0"
      |against|wider|
      |entertain|unterhalten|
      |teach|unterrichten|
      |winner|Siegerin|
      |en route|unterwegs|
      |full|satt|
      |full|voll|
      |interrupted|unterbrochen|
      |contract|Vertrag|
      |rides|reitet|
      |on|auf|
      |the|der|
      |#quizGroupPartition numCorrectResponsesInARow="3" repetitionInterval="15"
      |sweeps|streicht|100,200,300;405
      |""".stripMargin,
    """#quizGroup type="WordMapping" promptType="German word" responseType="English word" isActive="true" currentPromptNumber="0"
      |#quizGroupPartition numCorrectResponsesInARow="0" repetitionInterval="0"
      |unterwegs|en route|
      |#quizGroupPartition numCorrectResponsesInARow="1" repetitionInterval="5"
      |Vertrag|treaty|796;798
      |#quizGroupPartition numCorrectResponsesInARow="2" repetitionInterval="15"
      |Vertrag|contract|697,696;698
      |""".stripMargin
  )

  val quiz = Quiz.demoQuiz(quizData)
  val qghEngGer = QuizGroupHeader(WordMapping, "English word", "German word", "|", 4)
  val qghGerEng = QuizGroupHeader(WordMapping, "German word", "English word", "|", 4)
}
