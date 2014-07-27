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

import com.oranda.libanius.model.quizitem.{TextValue, QuizItem}
import com.oranda.libanius.model.wordmapping.{WordMappingValue, WordMappingValueSet, Dictionary}
import com.oranda.libanius.model.quizgroup._

object TestData {

  val qgCustomFormat =
    "#quizGroupPartition numCorrectResponsesInARow=\"0\" repetitionInterval=\"0\"\n" +
        "en route|unterwegs|\n" +
        "full|satt|\n" +
        "full|voll|\n" +
        "interrupted|unterbrochen|\n" +
        "contract|Vertrag|\n" +
        "rides|reitet|\n" +
        "on|auf|\n" +
        "sweeps|streicht|\n" +
        "#quizGroupPartition numCorrectResponsesInARow=\"1\" repetitionInterval=\"5\"\n" +
        "entertain|unterhalten|8;2\n" +
        "winner|Siegerin|5;0\n" +
        "#quizGroupPartition numCorrectResponsesInARow=\"2\" repetitionInterval=\"15\"\n" +
        "against|wider|9,7;6\n" +
        "teach|unterrichten|4,3;1\n" +
        "#quizGroupPartition numCorrectResponsesInARow=\"3\" repetitionInterval=\"15\"\n" +
        "#quizGroupPartition numCorrectResponsesInARow=\"4\" repetitionInterval=\"60\"\n" +
        "#quizGroupPartition numCorrectResponsesInARow=\"5\" repetitionInterval=\"600\"\n" +
        "#quizGroupPartition numCorrectResponsesInARow=\"6\" repetitionInterval=\"0\"\n"

  val qgwhCustomFormat =
      "#quizGroup type=\"WordMapping\" promptType=\"English word\" responseType=\"German word\" mainSeparator=\"|\" useMultipleChoiceUntil=\"4\" currentPromptNumber=\"10\" isActive=\"true\"\n" +
      qgCustomFormat

  def makeQgMemLevel0: QuizGroupMemoryLevel = QuizGroupMemoryLevel(0, List(
    QuizItem("en route", "unterwegs"),
    QuizItem("full", "satt"),
    QuizItem("full", "voll"),
    QuizItem("interrupted", "unterbrochen"),
    QuizItem("contract", "Vertrag"),
    QuizItem("rides", "reitet"),
    QuizItem("on", "auf"),
    QuizItem("sweeps", "streicht")).toStream)

  def makeQgMemLevel1: QuizGroupMemoryLevel = QuizGroupMemoryLevel(5, List(
    QuizItem("entertain", "unterhalten", List(8), List(2)),
    QuizItem("winner", "Siegerin", List(5), List(0))).toStream)

  def makeQgMemLevel2: QuizGroupMemoryLevel = QuizGroupMemoryLevel(15, List(
    QuizItem("against", "wider", List(9, 7), List(6)),
    QuizItem("teach", "unterrichten", List(4, 3), List(1))).toStream)

  def makeQuizGroup = QuizGroup(
    Map(0 -> makeQgMemLevel0, 1-> makeQgMemLevel1, 2-> makeQgMemLevel2),
    QuizGroupUserData(true, 10), new Dictionary())
  val quizGroup = makeQuizGroup

  def makeSimpleQuizGroup = QuizGroup(Map(0 -> makeQgMemLevel0), QuizGroupUserData(true, 0))
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

  def updatedWithUserResponse(qg: QuizGroup, quizItem: QuizItem): QuizGroup = {
    val userResponse = new UserResponse(qg.currentPromptNumber)
    val wasCorrect = true
    val quizItemUpdated = quizItem.updatedWithUserResponse(
      quizItem.correctResponse, wasCorrect, userResponse)
    val prevMemLevel = quizItemUpdated.numCorrectResponsesInARow
    qg.updateWithQuizItem(quizItemUpdated, wasCorrect, prevMemLevel)
  }

  def pullQuizItemAndAnswerCorrectly(qgwh: QuizGroupWithHeader): QuizGroupWithHeader = {
    val quizItem = qgwh.findPresentableQuizItem.get.quizItem
    QuizGroupWithHeader(qgwh.header, updatedWithUserResponse(qgwh.quizGroup, quizItem))
  }


  // QuizItem data

  val correctAnswersInARow = List(UserResponse(9), UserResponse(7))
  val incorrectAnswers = List(UserResponse(6))
  val userResponses = UserResponses(correctAnswersInARow, incorrectAnswers)
  val quizItem = QuizItem(TextValue("solve"), TextValue("nachlösen"), userResponses)

  // Memory level
  /*
   * Construct a quiz group partition.
   */
  def makeQgMemLevel: QuizGroupMemoryLevel = QuizGroupMemoryLevel(0, List(
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

  def makeQgMemLevelSimple: QuizGroupMemoryLevel = QuizGroupMemoryLevel(0, List(
    QuizItem("against", "wider"),
    QuizItem("entertain", "unterhalten")).toStream)


  // defaults for read-only
  val qgMemLevel = makeQgMemLevel
  val qgMemLevelSimple = makeQgMemLevelSimple

  val qgMemLevelSimpleCustomFormat =
      "against|wider|\n" +
      "entertain|unterhalten|\n"

  // word-mapping value set
  val wmvsCustomFormat = "contract|698,696;697/treaty|796;798"
  val wmvs = WordMappingValueSet.fromCustomFormat(wmvsCustomFormat, "|")

  // word-mapping value
  val wmvCustomFormat = "nachlösen|9,7;6"
  val wmv = WordMappingValue.fromCustomFormat(wmvCustomFormat, "|")
}