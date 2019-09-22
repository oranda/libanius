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

import com.oranda.libanius.model.quizgroup._
import java.lang.StringBuilder
import com.oranda.libanius.model.quizitem.QuizItem
import com.oranda.libanius.model.quizitem.TextValueOps.TextValue
import com.oranda.libanius.model.wordmapping._
import com.oranda.libanius.util.{StringSplitter, Util, StringUtil}
import scala.util.Try
import scala.collection.mutable.ListBuffer
import com.oranda.libanius.model.wordmapping.WordMappingPair
import com.oranda.libanius.model.wordmapping.WordMappingValueSetLazyProxy
import com.oranda.libanius.dependencies.AppDependencyAccess
import scala.collection.immutable.Stream
import com.oranda.libanius.model._

/**
 * Type class definition for ModelComponent serialization/deserialization using a custom format.
 */
trait CustomFormat[A <: ModelComponent, B <: ToParams, C <: FromParams]
  extends ToCustomFormat[A, B] with FromCustomFormat[A, C]

trait FromCustomFormat[A <: ModelComponent, B <: FromParams] {
  def from(str: String, extraParams: B): A
}

trait ToCustomFormat[A <: ModelComponent, B <: ToParams] {
  def to(component: A, strBuilder: StringBuilder, extraParams: B): StringBuilder
}

trait FromParams
trait ToParams

case class NoParams() extends FromParams with ToParams {
  def withSeparator(separator: String) = Separator(separator)
}

case class Separator(mainSeparator: String) extends ToParams with FromParams {
  def withIndex(index: Int) = SeparatorAndIndex(mainSeparator, index)
  def withIndexAndRepetitionInterval(index: Int, repetitionInterval: Int) =
    SeparatorIndexAndRepetitionInterval(this, index, repetitionInterval)
  override def toString = mainSeparator
}

case class SeparatorAndIndex(mainSeparator: String, index: Int) extends ToParams {
  def withoutIndex = Separator(mainSeparator)
}

case class SeparatorIndexAndRepetitionInterval(
    separator: Separator,
    index: Int,
    repetitionInterval: Int)
  extends FromParams

object SeparatorIndexAndRepetitionInterval {
  def apply(
      separator: String,
      index: Int,
      repetitionInterval: Int): SeparatorIndexAndRepetitionInterval =
    SeparatorIndexAndRepetitionInterval(Separator(separator), index, repetitionInterval)
}


// provides external access to the typeclass, forwarding the call to the appropriate type
object CustomFormat {

  def serialize[A <: ModelComponent, B <: ToParams](
      component: A,
      strBuilder: StringBuilder,
      params: B)
      (implicit customFormat: ToCustomFormat[A, B]): StringBuilder =
    customFormat.to(component, strBuilder, params)

  //TODO: @deprecated("Use CustomFormatParserFast for deserialization", since = "v0.984")
  def deserialize[A <: ModelComponent, B <: FromParams](
      str: String,
      fromParams: B)
      (implicit customFormat: FromCustomFormat[A, B]): A =
    customFormat.from(str, fromParams)
}

object CustomFormatForModelComponents {

  implicit object customFormatQuizGroupHeader
    extends CustomFormat[QuizGroupHeader, NoParams, NoParams] with AppDependencyAccess {

    /**
     * StringBuilder holds mutable state, but the serialization needs to be
     * highly efficient on Android for large quiz files.
     */
    def to(qgh: QuizGroupHeader, strBuilder: StringBuilder, params: NoParams) =
      strBuilder.append("#quizGroup type=\"").append(qgh.quizGroupType).
          append("\" promptType=\""). append(qgh.promptType).
          append("\" responseType=\"").append(qgh.responseType).
          append("\" mainSeparator=\"").append(qgh.mainSeparator).
          append("\" useMultipleChoiceUntil=\"").append(qgh.useMultipleChoiceUntil).
          append("\"")

    @deprecated("Use CustomFormatParserFast for deserialization", since = "v0.984")
    def from(str: String, params: NoParams) =
      QuizGroupHeader(
          parseQuizGroupType(str),
          parsePromptType(str),
          parseResponseType(str),
          Separator(parseMainSeparator(str)),
          parseUseMultipleChoiceUntil(str))

    private[this] def parseQuizGroupType(str: String): QuizGroupType =
      quizGroupType(StringUtil.parseValue(str, "type=\"", "\"").getOrElse(""))

    private[this] def quizGroupType(str: String): QuizGroupType =
      str match {
        case "WordMapping" => WordMapping
        case "QuestionAndAnswer" => QuestionAndAnswer
        case _ =>
          l.logError(s"QuizGroupType $str not recognized")
          QuestionAndAnswer
      }

    private[this] def parsePromptType(str: String): String =
      StringUtil.parseValue(str, "promptType=\"", "\"").getOrElse("")

    private[this] def parseResponseType(str: String): String =
      StringUtil.parseValue(str, "responseType=\"", "\"").getOrElse("")

    private[this] def parseMainSeparator(str: String): String =
      StringUtil.parseValue(str, "mainSeparator=\"", "\"").getOrElse("|")

    private[this] def parseUseMultipleChoiceUntil(str: String): Int =
      StringUtil.parseValue(str, "useMultipleChoiceUntil=\"", "\"").getOrElse("4").toInt

  }

  implicit object customFormatQuizGroupUserData
      extends CustomFormat[QuizGroupUserData, NoParams, NoParams] with AppDependencyAccess {

    def to(qgud: QuizGroupUserData, strBuilder: StringBuilder, params: NoParams) =
      strBuilder.append(" isActive=\"").append(qgud.isActive).append("\"").
          append(" currentPromptNumber=\"").append(qgud.currentPromptNumber).append("\"")

    @deprecated("Use CustomFormatParserFast for deserialization", since = "v0.984")
    def from(str: String, fromParams: NoParams) =
      QuizGroupUserData(parseIsActive(str), parseCurrentPromptNumber(str))

    private[this] def parseIsActive(str: String): Boolean =
      Try(StringUtil.parseValue(str, "isActive=\"", "\"").get.toBoolean).recover {
        case e: Exception =>
          l.logError(s"Could not parse isActive from $str")
          false
      }.get

    private[this] def parseCurrentPromptNumber(str: String): Int =
      Try(StringUtil.parseValue(str, "currentPromptNumber=\"", "\"").get.toInt).recover {
        case e: Exception =>
          l.logError(s"Could not parse prompt number from $str")
          0
      }.get
  }

  implicit object customFormatQuizGroupWithHeader
    extends CustomFormat[QuizGroupWithHeader, NoParams, Separator] {

    /*
     * Example of custom format:
     *
     * quizGroup type="WordMapping" promptType="English word" responseType="German word" currentPromptNumber="0" isActive="true"
     *    against|wider
     *    entertain|unterhalten
     */
    def to(
        qgwh: QuizGroupWithHeader,
        strBuilder: StringBuilder,
        extraParams: NoParams): StringBuilder = {
      customFormatQuizGroupHeader.to(qgwh.header, strBuilder, extraParams)
      customFormatQuizGroupUserData.to(qgwh.quizGroup.userData, strBuilder, extraParams)
      strBuilder.append('\n')
      customFormatQuizGroup.to(qgwh.quizGroup, strBuilder, qgwh.header.mainSeparator)
    }

    @deprecated("Use CustomFormatParserFast for deserialization", since = "v0.984")
    def from(text: String, fromParams: Separator): QuizGroupWithHeader = {
      val headerLine = text.takeWhile(_ != '\n')
      val qgHeader = QuizGroupHeader(headerLine)
      val qg = Util.stopwatch(customFormatQuizGroup.from(text,
          Separator(fromParams.mainSeparator)), "QuizGroup.fromCustomFormat")
      QuizGroupWithHeader(qgHeader, qg)
    }
  }

  implicit object customFormatQuizItem
      extends CustomFormat[QuizItem, Separator, Separator] {
    def to(qi: QuizItem, strBuilder: StringBuilder, extraParams: Separator): StringBuilder = {
      strBuilder.append(qi.prompt).append(extraParams.mainSeparator).
          append(qi.correctResponse).append(extraParams.mainSeparator)
      customFormatUserResponses.to(qi.userResponses, strBuilder, NoParams())
    }

    @deprecated("Use CustomFormatParserFast for deserialization", since = "v0.984")
    def from(strPromptResponse: String, fromParams: Separator): QuizItem = {
      val i = strPromptResponse.indexOf(fromParams.mainSeparator)
      val strPrompt = strPromptResponse.substring(0, i).trim
      val separatorLength = fromParams.mainSeparator.length
      val strResponseAndUserInfo = strPromptResponse.substring(i + separatorLength)

      val wmv = customFormatWordMappingValue.from(strResponseAndUserInfo, fromParams)
      val userResponses = UserResponsesAll(wmv.correctAnswersInARow, wmv.incorrectAnswers)
      QuizItem(strPrompt, wmv.value, userResponses)
    }
  }

  // Example: contract:696,697;698/treaty:796;798
  implicit object customFormatWordMappingValueSet
    extends CustomFormat[WordMappingValueSet, Separator, Separator]
    with AppDependencyAccess {

    def to(
        wmvs: WordMappingValueSet,
        strBuilder: StringBuilder,
        extraParams: Separator): StringBuilder = {
      def wmvToCustomFormat(strBuilder: StringBuilder, wmv: WordMappingValue):
          StringBuilder =
        customFormatWordMappingValue.to(wmv, strBuilder, extraParams)

      StringUtil.mkString(strBuilder, wmvs.values, wmvToCustomFormat, '/')
    }

    // Example: contract:696,697;698/treaty:796;798
    @deprecated("Use CustomFormatParserFast for deserialization", since = "v0.984")
    def from(str: String, fromParams: Separator): WordMappingValueSet = {
      val values = new ListBuffer[WordMappingValue]()
      val wmvsSplitter = stringSplitterFactory.getSplitter('/')

      def parseFromCustomFormat(): Unit = {
        wmvsSplitter.setString(str)
        while (wmvsSplitter.hasNext) {
          val nextVal = wmvsSplitter.next
          values += customFormatWordMappingValue.from(nextVal, fromParams)
        }
      }
      Try(parseFromCustomFormat) recover {
        case e: Exception => l.logError(s"WordMappingValueSet: Could not parse text $str", e)
      }
      WordMappingValueSet(values.toList)
    }
  }

  // Example: nachlösen:1,7,9;6
  implicit object customFormatWordMappingValue
    extends CustomFormat[WordMappingValue, Separator, Separator]
    with AppDependencyAccess {

    def to(
        wmv: WordMappingValue,
        strBuilder: StringBuilder,
        extraParams: Separator): StringBuilder = {
      strBuilder.append(wmv.value)

      if (wmv.correctAnswersInARow.nonEmpty || !wmv.incorrectAnswers.isEmpty)
        strBuilder.append(extraParams.mainSeparator)
      if (wmv.correctAnswersInARow.nonEmpty)
        StringUtil.mkString(strBuilder, wmv.correctAnswersInARow, wmv.answerPromptNumber, ',')
      if (wmv.incorrectAnswers.nonEmpty) {
        strBuilder.append(';')
        StringUtil.mkString(strBuilder, wmv.incorrectAnswers, wmv.answerPromptNumber, ',')
      }
      strBuilder
    }

    // Example: text = "nachlösen|1,7,9;6"
    @deprecated("Use CustomFormatParserFast for deserialization", since = "v0.984")
    def from(str: String, fromParams: Separator): WordMappingValue = {

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

    private[this] def parseAnswers(strAllAnswers: String): (List[String], List[String]) = {
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
    extends CustomFormat[UserResponsesAll, NoParams, Separator] {

    @deprecated("Use CustomFormatParserFast for deserialization", since = "v0.984")
    def from(str: String, fromParams: Separator): UserResponsesAll = {
      val wmv = customFormatWordMappingValue.from(str, fromParams)
      UserResponsesAll(wmv.correctAnswersInARow, wmv.incorrectAnswers)
    }

    def to(
        ur: UserResponsesAll,
        strBuilder: StringBuilder,
        extraParams: NoParams): StringBuilder = {
      if (ur.correctResponsesInARow.nonEmpty)
        StringUtil.mkString(strBuilder, ur.correctResponsesInARow, ur.responsePromptNumber, ',')
      if (ur.incorrectResponses.nonEmpty) {
        strBuilder.append(';')
        StringUtil.mkString(strBuilder, ur.incorrectResponses, ur.responsePromptNumber, ',')
      }
      strBuilder
    }
  }

  implicit object customFormatQuizGroup extends CustomFormat[QuizGroup, Separator, Separator] {

    def to(qg: QuizGroup, strBuilder: StringBuilder, extraParams: Separator): StringBuilder = {
      qg.levels.zipWithIndex.toStream.foreach { case (level, idx) =>
        customFormatQuizGroupMemoryLevel.to(level, strBuilder, extraParams.withIndex(idx))
      }
      strBuilder
    }

    /*
     * Text includes header line
     */
    @deprecated("Use CustomFormatParserFast for deserialization", since = "v0.984")
    def from(text: String, fromParams: Separator): QuizGroup = {

      val quizGroupParts = text.split("#quizGroupPartition ")
      val headerLine = quizGroupParts.head
      val quizGroupLevels = quizGroupParts.tail

      def parseMemLevelText(memLevelText: String): Tuple2[Int, QuizGroupMemoryLevel] = {
        val headerLine = memLevelText.takeWhile(_ != '\n')
        val mainMemLevelText = memLevelText.dropWhile(_ != '\n').tail
        val index = StringUtil.parseValue(headerLine,
            "numCorrectResponsesInARow=\"", "\"").getOrElse("0").toInt
        val repetitionInterval = StringUtil.parseValue(headerLine,
            "repetitionInterval=\"", "\"").getOrElse("0").toInt
        val memLevel = customFormatQuizGroupMemoryLevel.from(mainMemLevelText,
            fromParams.withIndexAndRepetitionInterval(index, repetitionInterval))
        (index, memLevel)
      }

      val levelsMap = quizGroupLevels.map(parseMemLevelText).toMap
      val userData: QuizGroupUserData = customFormatQuizGroupUserData.from(headerLine, NoParams())
      QuizGroup.createFromMemLevels(levelsMap, userData)
    }
  }

  implicit object customFormatQuizGroupMemoryLevel
    extends CustomFormat[QuizGroupMemoryLevel, SeparatorAndIndex, SeparatorIndexAndRepetitionInterval]
    with AppDependencyAccess {

    def to(qgml: QuizGroupMemoryLevel, strBuilder: StringBuilder,
        extraParams: SeparatorAndIndex) = {
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
    @deprecated("Use CustomFormatParserFast for deserialization", since = "v0.984")
    def from(
        text: String,
        fromParams: SeparatorIndexAndRepetitionInterval): QuizGroupMemoryLevel = {

      def parseQuizItem(strPromptResponse: String): Option[QuizItem] = {
        Try(Option(customFormatQuizItem.from(strPromptResponse,
            Separator(fromParams.separator.toString)))).recover {
          case e: Exception =>
            l.logError("could not parse quiz item with text " +
              s"$strPromptResponse using separator ${fromParams.separator.toString}")
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

      QuizGroupMemoryLevel(fromParams.index, fromParams.repetitionInterval, quizItems)
    }
  }

  implicit object customFormatDictionary extends CustomFormat[Dictionary, Separator, NoParams] {

    /*
     * Currently, dictionaries are written by external scripts, and are read-only within
     * a running Libanius app, so this function is not implemented.
     */
    def to(component: Dictionary, strBuilder: StringBuilder, params: Separator) =
      ???

    /*
     * Example:
     *
     * quizGroup type="WordMapping" promptType="English word" responseType="German word"
     *    against|wider
     *    entertain|unterhalten
     */
    @deprecated("Use CustomFormatParserFast for deserialization", since = "v0.984")
    def from(str: String, fromParams: NoParams): Dictionary =

      new Dictionary() {

        def parseCustomFormat() = {

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
          case e: Exception =>
            l.logError(s"Could not parse dictionary: ${e.getMessage}", e)
            None
        }
      }
    }

  implicit object customFormatWordMappingGroup
    extends CustomFormat[WordMappingGroup, Separator, Separator]
    with AppDependencyAccess {

    /*
     * Not used, as WordMappingGroup is only used within Dictionary, which is not written to.
     */
    def to(component: WordMappingGroup, strBuilder: StringBuilder, params: Separator) =
      ???

    /*
     * Example:
     *
     * quizGroup type="WordMapping" promptType="English word" responseType="German word" currentPromptNumber="0" isActive="true"
     *     against|wider
     *     entertain|unterhalten
     */
    @deprecated("Use CustomFormatParserFast for deserialization", since = "v0.984")
    def from(str: String, fromParams: Separator): WordMappingGroup = {

      val splitterLineBreak = stringSplitterFactory.getSplitter('\n')
      val wordMappingsMutable = new ListBuffer[WordMappingPair]()

      def parseQuizGroup(): Unit = {
        splitterLineBreak.setString(str)
        splitterLineBreak.next // skip the first line, which has already been parsed

        while (splitterLineBreak.hasNext) {
          val strPromptResponse = splitterLineBreak.next

          def parsePromptResponse = {

            val i = strPromptResponse.indexOf(fromParams.mainSeparator)
            val strPrompt = strPromptResponse.substring(0, i).trim
            val separatorLength = fromParams.mainSeparator.length
            val strResponseAndUserInfo = strPromptResponse.substring(i + separatorLength)

            wordMappingsMutable += WordMappingPair(strPrompt,
                WordMappingValueSetLazyProxy(strResponseAndUserInfo, fromParams.mainSeparator))
          }

          Try(parsePromptResponse) recover {
            case e: Exception =>
              l.logError(s"could not parse prompt-response string: $strPromptResponse")
          }
        }
      }
      Try(parseQuizGroup) recover {
        case e: Exception =>
          l.logError(s"could not parse wmg with text ${str.take(100)}...${str.takeRight(100)})")
      }

      val wordMappingsStream = wordMappingsMutable.toStream

      // Now use the persistent data structure.
      new WordMappingGroup(QuizGroupHeader(str), wordMappingsStream, QuizGroupUserData(str))
    }
  }
}
