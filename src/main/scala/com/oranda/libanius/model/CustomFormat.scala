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

import com.oranda.libanius.model.quizgroup._
import java.lang.StringBuilder
import com.oranda.libanius.model.quizitem.{TextValue, QuizItem}
import com.oranda.libanius.model.wordmapping._
import com.oranda.libanius.util.{StringSplitter, Util, StringUtil}
import scala.util.Try
import scala.collection.mutable.ListBuffer
import com.oranda.libanius.model.wordmapping.WordMappingPair
import com.oranda.libanius.model.wordmapping.WordMappingValueSetLazyProxy
import com.oranda.libanius.dependencies.AppDependencyAccess
import scala.collection.immutable.Stream

/**
 * Type class definition for ModelComponent serialization/deserialization using a custom format.
 */
trait CustomFormat[A <: ModelComponent, B <: Params, C <: FromParams]
  extends ToCustomFormat[A, B] with FromCustomFormat[A, C]

trait FromCustomFormat[A <: ModelComponent, C <: FromParams] {
  def from(str: String, extraParams: C): A
}

trait ToCustomFormat[A <: ModelComponent, B <: Params] {
  def to(component: A, strBuilder: StringBuilder, extraParams: B): StringBuilder
}

trait FromParams {}

case class FromParamsDefault() extends FromParams
case class FromParamsWithSeparator(mainSeparator: String) extends FromParams {
  def withRepetitionInterval(repetitionInterval: Int) =
    FromParamsWithSeparatorAndRepetitionInterval(mainSeparator, repetitionInterval)
}
case class FromParamsWithSeparatorAndRepetitionInterval(mainSeparator: String,
    repetitionInterval: Int) extends FromParams

trait Params {}

case class ParamsDefault() extends Params {
  def withSeparator(separator: String) = ParamsWithSeparator(separator)
}

case class ParamsWithSeparator(mainSeparator: String) extends Params {
  def withIndex(index: Int) = ParamsWithSeparatorAndIndex(mainSeparator, index)
}

case class ParamsWithSeparatorAndIndex(mainSeparator: String, index: Int) extends Params {
  def withoutIndex = ParamsWithSeparator(mainSeparator)
}

// provides external access to the typeclass, forwarding the call to the appropriate type
object CustomFormat {

  def to[A <: ModelComponent, B <: Params](component: A,
      strBuilder: StringBuilder, params: B)
      (implicit customFormat: ToCustomFormat[A, B]): StringBuilder =
    customFormat.to(component, strBuilder, params)

  def from[A <: ModelComponent, B <: FromParams](str: String,
      fromParams: B)(implicit customFormat: FromCustomFormat[A, B]): A =
    customFormat.from(str, fromParams)
}

object CustomFormatForModelComponents {
  implicit object customFormatQuizGroupHeader
      extends CustomFormat[QuizGroupHeader, ParamsDefault, FromParams] {

    def to(qgh: QuizGroupHeader, strBuilder: StringBuilder, params: ParamsDefault) =
      strBuilder.append("#quizGroup type=\"").append(qgh.quizGroupType).
          append("\" promptType=\""). append(qgh.promptType).
          append("\" responseType=\"").append(qgh.responseType).
          append("\" mainSeparator=\"").append(qgh.mainSeparator).
          append("\" useMultipleChoiceUntil=\"").append(qgh.useMultipleChoiceUntil).
          append("\"")

    def from(str: String, fromParams: FromParams) = ???
  }

  implicit object customFormatQuizGroupUserData
      extends CustomFormat[QuizGroupUserData, ParamsDefault, FromParams] {

    def to(qgud: QuizGroupUserData, strBuilder: StringBuilder, params: ParamsDefault) =
      strBuilder.append(" currentPromptNumber=\"").
          append(qgud.currentPromptNumber).append("\"").append(" isActive=\"").
          append(qgud.isActive).append("\"")

    def from(str: String, fromParams: FromParams) = ???
  }

  implicit object customFormatQuizGroupWithHeader
      extends CustomFormat[QuizGroupWithHeader, ParamsDefault, FromParamsWithSeparator] {
    /*
     * Example of custom format:
     *
     * quizGroup type="WordMapping" promptType="English word" responseType="German word" currentPromptNumber="0" isActive="true"
     *    against|wider
     *    entertain|unterhalten
     */
    def to(qgwh: QuizGroupWithHeader, strBuilder: StringBuilder, extraParams: ParamsDefault):
        StringBuilder = {
      customFormatQuizGroupHeader.to(qgwh.header, strBuilder, extraParams)
      customFormatQuizGroupUserData.to(qgwh.quizGroup.userData, strBuilder, extraParams)
      strBuilder.append('\n')
      customFormatQuizGroup.to(qgwh.quizGroup, strBuilder,
          extraParams.withSeparator(qgwh.header.mainSeparator))
    }

    def from(text: String, fromParams: FromParamsWithSeparator): QuizGroupWithHeader = {
      val headerLine = text.takeWhile(_ != '\n')
      val qgHeader = QuizGroupHeader(headerLine)
      val qg = Util.stopwatch(customFormatQuizGroup.from(text,
          FromParamsWithSeparator(fromParams.mainSeparator)), "QuizGroup.fromCustomFormat")
      QuizGroupWithHeader(qgHeader, qg)
    }
  }

  implicit object customFormatQuizItem
      extends CustomFormat[QuizItem, ParamsWithSeparator, FromParamsWithSeparator] {
    def to(qi: QuizItem, strBuilder: StringBuilder, extraParams: ParamsWithSeparator):
        StringBuilder = {
      strBuilder.append(qi.prompt).append(extraParams.mainSeparator).
          append(qi.correctResponse).append(extraParams.mainSeparator)
      customFormatUserResponses.to(qi.userResponses, strBuilder, ParamsDefault())
    }

    def from(strPromptResponse: String, fromParams: FromParamsWithSeparator): QuizItem = {
      val i = strPromptResponse.indexOf(fromParams.mainSeparator)
      val strPrompt = strPromptResponse.substring(0, i).trim
      val strResponseAndUserInfo = strPromptResponse.substring(i +
          fromParams.mainSeparator.length)

      val wmv = customFormatWordMappingValue.from(strResponseAndUserInfo, fromParams)
      val userResponses = UserResponses(wmv.correctAnswersInARow, wmv.incorrectAnswers)
      QuizItem(TextValue(strPrompt), TextValue(wmv.value), userResponses)
    }
  }

  // Example: contract:696,697;698/treaty:796;798
  implicit object customFormatWordMappingValueSet
      extends CustomFormat[WordMappingValueSet, ParamsWithSeparator, FromParamsWithSeparator]
      with AppDependencyAccess {
    def to(wmvs: WordMappingValueSet, strBuilder: StringBuilder,
        extraParams: ParamsWithSeparator): StringBuilder = {
      def wmvToCustomFormat(strBuilder: StringBuilder, wmv: WordMappingValue):
          StringBuilder =
        customFormatWordMappingValue.to(wmv, strBuilder, extraParams)

      StringUtil.mkString(strBuilder, wmvs.values, wmvToCustomFormat, '/')
    }

    // Example: contract:696,697;698/treaty:796;798
    def from(str: String, fromParams: FromParamsWithSeparator): WordMappingValueSet = {
      val values = new ListBuffer[WordMappingValue]()

      val wmvsSplitter = stringSplitterFactory.getSplitter('/')
      def parseFromCustomFormat {
        wmvsSplitter.setString(str)
        while (wmvsSplitter.hasNext) {
          val nextVal = wmvsSplitter.next
          values += customFormatWordMappingValue.from(nextVal, fromParams)
        }
      }
      Try(parseFromCustomFormat) recover {
        case e: Exception => l.logError("WordMappingValueSet: Could not parse text " + str, e)
      }
      WordMappingValueSet(values.toList)
    }
  }

  // Example: nachlösen:1,7,9;6
  implicit object customFormatWordMappingValue
      extends CustomFormat[WordMappingValue, ParamsWithSeparator, FromParamsWithSeparator]
      with AppDependencyAccess {
    def to(wmv: WordMappingValue, strBuilder: StringBuilder,
        extraParams: ParamsWithSeparator): StringBuilder = {
      strBuilder.append(wmv.value)

      if (!wmv.correctAnswersInARow.isEmpty || !wmv.incorrectAnswers.isEmpty)
        strBuilder.append(extraParams.mainSeparator)
      if (!wmv.correctAnswersInARow.isEmpty)
        StringUtil.mkString(strBuilder, wmv.correctAnswersInARow, wmv.answerPromptNumber, ',')
      if (!wmv.incorrectAnswers.isEmpty) {
        strBuilder.append(';')
        StringUtil.mkString(strBuilder, wmv.incorrectAnswers, wmv.answerPromptNumber, ',')
      }
      strBuilder
    }

    // Example: text = "nachlösen|1,7,9;6"
    def from(str: String, fromParams: FromParamsWithSeparator): WordMappingValue = {

      import com.oranda.libanius.util.StringUtil.RichString
      str.optionalIndex(fromParams.mainSeparator) match {
        case Some(index) =>
          val strResponse = str.substring(0, index)
          val strAllAnswers = str.substring(index + fromParams.mainSeparator.length)

          val wmv = WordMappingValue(strResponse.trim)
          if (strAllAnswers.isEmpty)
            wmv
          else {
            val (correctAnswers, incorrectAnswers) = parseAnswers(strAllAnswers)
            wmv.addUserAnswersBatch(correctAnswers, incorrectAnswers)
          }
        case None => WordMappingValue(str.trim)
      }
    }

    private def parseAnswers(strAllAnswers: String): (List[String], List[String]) = {
      // This code needs to be both fast and thread-safe so special "splitters" are used.
      val allAnswersSplitter = stringSplitterFactory.getSplitter(';')
      val answersSplitter = stringSplitterFactory.getSplitter(',')
      allAnswersSplitter.setString(strAllAnswers)

      val correctPromptNums = allAnswersSplitter.next
      answersSplitter.setString(correctPromptNums)
      val correctAnswers = answersSplitter.toList
      val incorrectAnswers =
        if (allAnswersSplitter.hasNext) {
          val incorrectPromptNums = allAnswersSplitter.next
          answersSplitter.setString(incorrectPromptNums)
          answersSplitter.toList
        } else
          Nil
      (correctAnswers, incorrectAnswers)
    }
  }

  // Example: 1,7,9;6
  implicit object customFormatUserResponses
      extends CustomFormat[UserResponses, ParamsDefault, FromParams] {

    def from(str: String, fromParams: FromParams): UserResponses = ???

    def to(ur: UserResponses, strBuilder: StringBuilder, extraParams: ParamsDefault):
        StringBuilder = {
      if (!ur.correctResponsesInARow.isEmpty)
        StringUtil.mkString(strBuilder, ur.correctResponsesInARow,
          ur.responsePromptNumber, ',')
      if (!ur.incorrectResponses.isEmpty) {
        strBuilder.append(';')
        StringUtil.mkString(strBuilder, ur.incorrectResponses, ur.responsePromptNumber, ',')
      }
      strBuilder
    }
  }

  implicit object customFormatQuizGroup
      extends CustomFormat[QuizGroup, ParamsWithSeparator, FromParamsWithSeparator] {

    def to(qg: QuizGroup, strBuilder: StringBuilder, extraParams: ParamsWithSeparator):
        StringBuilder = {
      for (memLevel <- qg.levels.zipWithIndex.toStream)
        customFormatQuizGroupMemoryLevel.to(memLevel._1, strBuilder,
            extraParams.withIndex(memLevel._2))
      strBuilder
    }

    /*
     * Text includes header line
     */
    def from(text: String, fromParams: FromParamsWithSeparator): QuizGroup = {

      val quizGroupParts = text.split("#quizGroupPartition ")
      val headerLine = quizGroupParts.head
      val quizGroupLevels = quizGroupParts.tail

      def parseMemLevelText(memLevelText: String): Pair[Int, QuizGroupMemoryLevel] = {
        val headerLine = memLevelText.takeWhile(_ != '\n')
        val mainMemLevelText = memLevelText.dropWhile(_ != '\n').tail
        val index = StringUtil.parseValue(headerLine,
            "numCorrectResponsesInARow=\"", "\"").getOrElse("0").toInt
        val repetitionInterval = StringUtil.parseValue(headerLine,
            "repetitionInterval=\"", "\"").getOrElse("0").toInt
        val memLevel = customFormatQuizGroupMemoryLevel.from(mainMemLevelText,
            fromParams.withRepetitionInterval(repetitionInterval))
        Pair(index, memLevel)
      }

      val levelsMap = quizGroupLevels.map(parseMemLevelText(_)).toMap
      val userData: QuizGroupUserData = QuizGroupUserData(headerLine)

      QuizGroup.createFromMemLevels(levelsMap, userData)
    }
  }

  implicit object customFormatQuizGroupMemoryLevel
      extends CustomFormat[QuizGroupMemoryLevel, ParamsWithSeparatorAndIndex, FromParamsWithSeparatorAndRepetitionInterval]
      with AppDependencyAccess {
    def to(qgml: QuizGroupMemoryLevel, strBuilder: StringBuilder,
        extraParams: ParamsWithSeparatorAndIndex) = {
      strBuilder.append("#quizGroupPartition numCorrectResponsesInARow=\"" +
          extraParams.index + "\" repetitionInterval=\"" + qgml.repetitionInterval + "\"" + '\n')

      for (quizItem <- qgml.quizItems.toStream) {
        customFormatQuizItem.to(quizItem, strBuilder, extraParams.withoutIndex)
        strBuilder.append('\n')
      }
      strBuilder
    }

    /*
     * Text does not include header line
     */
    def from(text: String, fromParams: FromParamsWithSeparatorAndRepetitionInterval): QuizGroupMemoryLevel = {

      def parseQuizItem(strPromptResponse: String): Option[QuizItem] = {
        Try(Some(customFormatQuizItem.from(strPromptResponse,
            FromParamsWithSeparator(fromParams.mainSeparator)))).recover {
          case e: Exception => l.logError("could not parse quiz item with text " +
            strPromptResponse + " using separator " + fromParams.mainSeparator)
            None
        }.get
      }

      def parseQuizItems(lineSplitter: StringSplitter): Stream[QuizItem] = {
        if (lineSplitter.hasNext)
          parseQuizItem(lineSplitter.next) match {
            case Some(line) => Stream.cons(line, parseQuizItems(lineSplitter))
            case _ => parseQuizItems(lineSplitter)
          }
        else
          Stream.empty
      }

      val lineSplitter = stringSplitterFactory.getSplitter('\n')
      lineSplitter.setString(text)
      val quizItems = parseQuizItems(lineSplitter)

      QuizGroupMemoryLevel(fromParams.repetitionInterval, quizItems)
    }
  }

  implicit object customFormatDictionary
      extends CustomFormat[Dictionary, ParamsWithSeparator, FromParamsDefault] {

    def to(component: Dictionary, strBuilder: StringBuilder, params: ParamsWithSeparator) =
      ???

    /*
     * Example:
     *
     * quizGroup type="WordMapping" promptType="English word" responseType="German word"
     *    against|wider
     *    entertain|unterhalten
     */
    def from(str: String, fromParams: FromParamsDefault): Dictionary =

      new Dictionary() {

        def parseCustomFormat = {

          val splitterLineBreak = stringSplitterFactory.getSplitter('\n')
          val splitterKeyValue = stringSplitterFactory.getSplitter('|')

          splitterLineBreak.setString(str)
          splitterLineBreak.next // skip the first line, which has already been parsed

          while (splitterLineBreak.hasNext) {
            splitterKeyValue.setString(splitterLineBreak.next)

            if (splitterKeyValue.hasNext) {
              val strKey = splitterKeyValue.next
              if (splitterKeyValue.hasNext) {
                val strValues = splitterKeyValue.next
                // for efficiency, avoid an extra method call into Dictionary here
                wordMappings.put(strKey, WordMappingValueSetLazyProxy(strValues, "|"))
              }
            }
          }
        }

        Try(parseCustomFormat) recover {
          case e: Exception => l.logError("Could not parse dictionary: " + e.getMessage(), e)
            None
        }
      }
    }

  implicit object customFormatWordMappingGroup
      extends CustomFormat[WordMappingGroup, ParamsWithSeparator, FromParamsWithSeparator]
      with AppDependencyAccess {

    def to(component: WordMappingGroup, strBuilder: StringBuilder, params: ParamsWithSeparator) =
      ???

    /*
     * Example:
     *
     * quizGroup type="WordMapping" promptType="English word" responseType="German word" currentPromptNumber="0" isActive="true"
     *     against|wider
     *     entertain|unterhalten
     */
    def from(str: String, fromParams: FromParamsWithSeparator): WordMappingGroup = {

      val splitterLineBreak = stringSplitterFactory.getSplitter('\n')
      val wordMappingsMutable = new ListBuffer[WordMappingPair]()

      def parseQuizGroup {
        splitterLineBreak.setString(str)
        splitterLineBreak.next // skip the first line, which has already been parsed

        while (splitterLineBreak.hasNext) {
          val strPromptResponse = splitterLineBreak.next

          def parsePromptResponse = {

            val i = strPromptResponse.indexOf(fromParams.mainSeparator)
            val strPrompt = strPromptResponse.substring(0, i).trim
            val strResponseAndUserInfo = strPromptResponse.substring(i +
                fromParams.mainSeparator.length)
            wordMappingsMutable += WordMappingPair(strPrompt,
              WordMappingValueSetLazyProxy(strResponseAndUserInfo, fromParams.mainSeparator))
          }

          Try(parsePromptResponse) recover {
            case e: Exception => l.logError("could not parse prompt-response string: " +
              strPromptResponse)
          }
        }
      }
      Try(parseQuizGroup) recover {
        case e: Exception => l.logError("could not parse wmg with text " + str.take(100) + "..." +
          str.takeRight(100))
      }

      val wordMappingsStream = wordMappingsMutable.toStream

      // Now use the persistent data structure.
      new WordMappingGroup(QuizGroupHeader(str), wordMappingsStream, QuizGroupUserData(str))
    }
  }
}