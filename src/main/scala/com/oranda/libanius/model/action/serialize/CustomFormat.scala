/*
 * Libanius
 * Copyright (C) 2012-2022 James McCabe <jjtmccabe@gmail.com>
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
import com.oranda.libanius.model.*
import com.oranda.libanius.model.quizgroup.*
import com.oranda.libanius.model.quizgroup.QuizGroupType.{QuestionAndAnswer, WordMapping}
import com.oranda.libanius.model.quizitem.QuizItem
import com.oranda.libanius.model.quizitem.TextValueOps.TextValue
import com.oranda.libanius.model.wordmapping.*
import com.oranda.libanius.util.{StringSplitter, StringUtil, Util}

import java.lang.StringBuilder
import scala.collection.mutable.ListBuffer
import scala.util.Try

/**
 * Type class definition for ModelComponent serialization/deserialization using
 * a custom format.
 */
trait CustomFormat[A <: ModelComponent, B <: ToParams, C <: FromParams]
    extends ToCustomFormat[A, B]
    with FromCustomFormat[A, C]

trait FromCustomFormat[A <: ModelComponent, B <: FromParams] {
  def from(str: String, extraParams: B): A
}

trait ToCustomFormat[A <: ModelComponent, B <: ToParams] {
  def to(component: A, strBuilder: StringBuilder, extraParams: B): StringBuilder
}

trait FromParams
trait ToParams

case class ParamsNone() extends FromParams with ToParams {
  def withSeparator(separator: String) = ParamsSeparator(separator)
}

case class ParamsSeparator(separator: String) extends ToParams with FromParams {
  override def toString = separator
}

case class QuizGroupMemoryLevelFromParams(separator: String, index: Int, repetitionInterval: Int)
  extends FromParams

case class QuizGroupMemoryLevelToParams(mainSeparator: String, index: Int) extends ToParams {
  def withoutIndex = ParamsSeparator(mainSeparator)
}

case class QuizGroupHeaderFromParams(
  separator: String,
  numCorrectResponsesRequired: Int,
  useMultipleChoiceUntil: Int
) extends FromParams

case class QuizGroupHeaderToParams(separator: String) extends ToParams {
  def withIndex(index: Int) = QuizGroupMemoryLevelToParams(separator, index)
  def withIndexAndRepetitionInterval(index: Int, repetitionInterval: Int) =
    QuizGroupHeaderFromParams(separator, index, repetitionInterval)
  override def toString = separator
}
// provides external access to the typeclass, forwarding the call to the appropriate type
object CustomFormat {
  def serialize[A <: ModelComponent, B <: ToParams](component: A, strBuilder: StringBuilder, params: B)(implicit
    customFormat: ToCustomFormat[A, B]
  ): StringBuilder =
    customFormat.to(component, strBuilder, params)

  def deserialize[A <: ModelComponent, B <: FromParams](str: String, fromParams: B)(implicit
    customFormat: FromCustomFormat[A, B]
  ): A = customFormat.from(str, fromParams)
}

object CustomFormatForModelComponents {
  implicit object customFormatQuizGroupHeader
      extends CustomFormat[QuizGroupHeader, ParamsNone, ParamsNone]
      with AppDependencyAccess {
    /**
     * StringBuilder holds mutable state, but the serialization needs to be
     * highly efficient on Android for large quiz files.
     */
    def to(qgh: QuizGroupHeader, strBuilder: StringBuilder, params: ParamsNone) =
      strBuilder
        .append("#quizGroup promptType=\"")
        .append(qgh.promptType)
        .append("\" responseType=\"")
        .append(qgh.responseType)
        .append("\" type=\"")
        .append(qgh.quizGroupType)
        .append("\" mainSeparator=\"")
        .append(qgh.mainSeparator)
        .append("\" numCorrectResponsesRequired=\"")
        .append(qgh.numCorrectResponsesRequired)
        .append("\" useMultipleChoiceUntil=\"")
        .append(qgh.useMultipleChoiceUntil)
        .append("\"")

    def from(str: String, params: ParamsNone) =
      QuizGroupHeader(
        parsePromptType(str),
        parseResponseType(str),
        parseQuizGroupType(str),
        parseMainSeparator(str),
        parseNumCorrectResponsesRequired(str),
        parseUseMultipleChoiceUntil(str)
      )

    private[this] def parseQuizGroupType(str: String): QuizGroupType =
      quizGroupType(StringUtil.parseValue(str, "type=\"", "\"").getOrElse(""))

    private[this] def quizGroupType(str: String): QuizGroupType =
      str match {
        case "WordMapping"       => WordMapping
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

    private[this] def parseNumCorrectResponsesRequired(str: String): Int =
      StringUtil.parseInt(str, "numCorrectResponsesRequired=\"", "\"").
        getOrElse(QuizGroupHeader.defaultNumCorrectResponsesRequired)

    private[this] def parseUseMultipleChoiceUntil(str: String): Int =
      StringUtil.parseInt(str, "useMultipleChoiceUntil=\"", "\"").
        getOrElse(QuizGroupHeader.defaultUseMultipleChoiceUntil)
  }

  implicit object customFormatQuizGroupUserData
      extends CustomFormat[QuizGroupUserData, ParamsNone, ParamsNone]
      with AppDependencyAccess {
    def to(qgud: QuizGroupUserData, strBuilder: StringBuilder, params: ParamsNone) =
      strBuilder
        .append(" isActive=\"")
        .append(qgud.isActive)
        .append("\"")
        .append(" currentPromptNumber=\"")
        .append(qgud.currentPromptNumber)
        .append("\"")

    def from(str: String, fromParams: ParamsNone) =
      QuizGroupUserData(parseIsActive(str), parseCurrentPromptNumber(str))

    private[this] def parseIsActive(str: String): Boolean =
      Try(StringUtil.parseValue(str, "isActive=\"", "\"").get.toBoolean).recover { case e: Exception =>
        l.logError(s"Could not parse isActive from $str")
        false
      }.get

    private[this] def parseCurrentPromptNumber(str: String): Int =
      Try(StringUtil.parseValue(str, "currentPromptNumber=\"", "\"").get.toInt).recover { case e: Exception =>
        l.logError(s"Could not parse prompt number from $str")
        0
      }.get
  }

  implicit object customFormatQuizGroupWithHeader extends CustomFormat[QuizGroupWithHeader, ParamsNone, ParamsSeparator] {
    /*
     * Example of custom format:
     *
     * quizGroup type="WordMapping" promptType="English word" type="QuestionAndAnswer" numCorrectResponsesRequired="4" useMultipleChoiceUntil="1" responseType="German word" currentPromptNumber="0" isActive="true"
     *    against|wider
     *    entertain|unterhalten
     */
    def to(qgwh: QuizGroupWithHeader, strBuilder: StringBuilder, extraParams: ParamsNone): StringBuilder = {
      customFormatQuizGroupHeader.to(qgwh.header, strBuilder, extraParams)
      customFormatQuizGroupUserData.to(qgwh.quizGroup.userData, strBuilder, extraParams)
      strBuilder.append('\n')
      val quizGroupHeaderToParams = QuizGroupHeaderToParams(qgwh.header.mainSeparator.separator)
      customFormatQuizGroup.to(qgwh.quizGroup, strBuilder, quizGroupHeaderToParams)
    }

    def from(text: String, fromParams: ParamsSeparator): QuizGroupWithHeader = {
      val headerLine = text.takeWhile(_ != '\n')
      val qgHeader   = QuizGroupHeader(headerLine)
      val qg = Util.stopwatch(
        customFormatQuizGroup.from(
          text,
          QuizGroupHeaderFromParams(
            fromParams.separator,
            qgHeader.numCorrectResponsesRequired,
            qgHeader.useMultipleChoiceUntil
          )
        ),
        "QuizGroup.fromCustomFormat"
      )
      QuizGroupWithHeader(qgHeader, qg)
    }
  }

  implicit object customFormatQuizItem extends CustomFormat[QuizItem, ParamsSeparator, ParamsSeparator] {
    def to(qi: QuizItem, strBuilder: StringBuilder, extraParams: ParamsSeparator): StringBuilder = {
      strBuilder
        .append(qi.prompt)
        .append(extraParams.separator)
        .append(qi.correctResponse)
        .append(extraParams.separator)
      customFormatUserResponses.to(qi.userResponses, strBuilder, ParamsNone())
    }

    def from(strPromptResponse: String, fromParams: ParamsSeparator): QuizItem = {
      val i                      = strPromptResponse.indexOf(fromParams.separator)
      val strPrompt              = strPromptResponse.substring(0, i).trim
      val separatorLength        = fromParams.separator.length
      val strResponseAndUserInfo = strPromptResponse.substring(i + separatorLength)

      val wmv           = customFormatWordMappingValue.from(strResponseAndUserInfo, fromParams)
      val userResponses = UserResponsesAll(wmv.correctAnswersInARow, wmv.incorrectAnswers)
      QuizItem(strPrompt, wmv.value, userResponses)
    }
  }

  // Example: contract:696,697;698/treaty:796;798
  implicit object customFormatWordMappingValueSet
      extends CustomFormat[WordMappingValueSet, ParamsSeparator, ParamsSeparator]
      with AppDependencyAccess {
    def to(wmvs: WordMappingValueSet, strBuilder: StringBuilder, extraParams: ParamsSeparator): StringBuilder = {
      def wmvToCustomFormat(strBuilder: StringBuilder, wmv: WordMappingValue): StringBuilder =
        customFormatWordMappingValue.to(wmv, strBuilder, extraParams)

      StringUtil.mkString(strBuilder, wmvs.values, wmvToCustomFormat, '/')
    }

    // Example: contract:696,697;698/treaty:796;798
    def from(str: String, fromParams: ParamsSeparator): WordMappingValueSet = {
      val values       = new ListBuffer[WordMappingValue]()
      val wmvsSplitter = stringSplitterFactory.getSplitter('/')

      def parseFromCustomFormat(): Unit = {
        wmvsSplitter.setString(str)
        while wmvsSplitter.hasNext do {
          val nextVal = wmvsSplitter.next
          values += customFormatWordMappingValue.from(nextVal, fromParams)
        }
      }
      Try(parseFromCustomFormat()) recover { case e: Exception =>
        l.logError(s"WordMappingValueSet: Could not parse text $str", e)
      }
      WordMappingValueSet(values.toList)
    }
  }

  // Example: nachlösen:1,7,9;6
  implicit object customFormatWordMappingValue
      extends CustomFormat[WordMappingValue, ParamsSeparator, ParamsSeparator]
      with AppDependencyAccess {
    def to(wmv: WordMappingValue, strBuilder: StringBuilder, extraParams: ParamsSeparator): StringBuilder = {
      strBuilder.append(wmv.value)

      if wmv.correctAnswersInARow.nonEmpty || !wmv.incorrectAnswers.isEmpty then
        strBuilder.append(extraParams.separator)
      if wmv.correctAnswersInARow.nonEmpty then
        StringUtil.mkString(strBuilder, wmv.correctAnswersInARow, wmv.answerPromptNumber, ',')
      if wmv.incorrectAnswers.nonEmpty then {
        strBuilder.append(';')
        StringUtil.mkString(strBuilder, wmv.incorrectAnswers, wmv.answerPromptNumber, ',')
      }
      strBuilder
    }

    // Example: text = "nachlösen|1,7,9;6"
    def from(str: String, fromParams: ParamsSeparator): WordMappingValue = {
      import com.oranda.libanius.util.StringUtil.RichString
      str.optionalIndex(fromParams.separator) match {
        case Some(index) =>
          val strResponse   = str.substring(0, index)
          val strAllAnswers = str.substring(index + fromParams.separator.length)

          val wmv = WordMappingValue(strResponse.trim)
          if strAllAnswers.isEmpty then wmv
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
      val answersSplitter    = stringSplitterFactory.getSplitter(',')
      allAnswersSplitter.setString(strAllAnswers)

      val correctPromptNums = allAnswersSplitter.next
      answersSplitter.setString(correctPromptNums)
      val correctAnswers = answersSplitter.toList
      val incorrectAnswers =
        if allAnswersSplitter.hasNext then {
          val incorrectPromptNums = allAnswersSplitter.next
          answersSplitter.setString(incorrectPromptNums)
          answersSplitter.toList
        } else Nil
      (correctAnswers, incorrectAnswers)
    }
  }

  // Example: 1,7,9;6
  implicit object customFormatUserResponses extends CustomFormat[UserResponsesAll, ParamsNone, ParamsSeparator] {
    def from(str: String, fromParams: ParamsSeparator): UserResponsesAll = {
      val wmv = customFormatWordMappingValue.from(str, fromParams)
      UserResponsesAll(wmv.correctAnswersInARow, wmv.incorrectAnswers)
    }

    def to(ur: UserResponsesAll, strBuilder: StringBuilder, extraParams: ParamsNone): StringBuilder = {
      if ur.correctResponsesInARow.nonEmpty then
        StringUtil.mkString(strBuilder, ur.correctResponsesInARow, ur.responsePromptNumber, ',')
      if ur.incorrectResponses.nonEmpty then {
        strBuilder.append(';')
        StringUtil.mkString(strBuilder, ur.incorrectResponses, ur.responsePromptNumber, ',')
      }
      strBuilder
    }
  }

  implicit object customFormatQuizGroup extends CustomFormat[
    QuizGroup,
    QuizGroupHeaderToParams,
    QuizGroupHeaderFromParams
  ] {
    def to(qg: QuizGroup, strBuilder: StringBuilder, extraParams: QuizGroupHeaderToParams): StringBuilder = {
      qg.levels.zipWithIndex.to(LazyList).foreach { case (level, idx) =>
        customFormatQuizGroupMemoryLevel.to(level, strBuilder, extraParams.withIndex(idx))
      }
      strBuilder
    }

    /*
     * Text includes header line
     */
    def from(text: String, fromParams: QuizGroupHeaderFromParams): QuizGroup = {
      val quizGroupParts  = text.split("#quizGroupPartition ")
      val headerLine      = quizGroupParts.head
      val quizGroupLevels = quizGroupParts.tail

      def parseMemLevelText(memLevelText: String): Tuple2[Int, QuizGroupMemoryLevel] = {
        val headerLine         = memLevelText.takeWhile(_ != '\n')
        val mainMemLevelText   = memLevelText.dropWhile(_ != '\n').tail
        val index              = StringUtil.parseValue(headerLine, "numCorrectResponsesInARow=\"", "\"").getOrElse("0").toInt
        val repetitionInterval = StringUtil.parseValue(headerLine, "repetitionInterval=\"", "\"").getOrElse("0").toInt
        val memLevel = customFormatQuizGroupMemoryLevel.from(
          mainMemLevelText,
          QuizGroupMemoryLevelFromParams(fromParams.separator, index, repetitionInterval)
        )
        (index, memLevel)
      }

      val levelsMap                          = quizGroupLevels.map(parseMemLevelText).toMap
      val userData: QuizGroupUserData        = customFormatQuizGroupUserData.from(headerLine, ParamsNone())
      QuizGroup.createFromMemLevels(levelsMap, userData, fromParams.numCorrectResponsesRequired)
    }
  }

  implicit object customFormatQuizGroupMemoryLevel
      extends CustomFormat[QuizGroupMemoryLevel, QuizGroupMemoryLevelToParams, QuizGroupMemoryLevelFromParams]
      with AppDependencyAccess {
    def to(qgml: QuizGroupMemoryLevel, strBuilder: StringBuilder, extraParams: QuizGroupMemoryLevelToParams) = {
      strBuilder.append(
        "#quizGroupPartition numCorrectResponsesInARow=\"" +
          extraParams.index + "\" repetitionInterval=\"" + qgml.repetitionInterval + "\"" + '\n'
      )

      for quizItem <- qgml.quizItems.to(LazyList) do {
        customFormatQuizItem.to(quizItem, strBuilder, extraParams.withoutIndex)
        strBuilder.append('\n')
      }
      strBuilder
    }

    /*
     * Text does not include header line
     */
    def from(text: String, fromParams: QuizGroupMemoryLevelFromParams): QuizGroupMemoryLevel = {
      def parseQuizItem(strPromptResponse: String): Option[QuizItem] =
        Try(Option(customFormatQuizItem.from(strPromptResponse, ParamsSeparator(fromParams.separator.toString)))).recover {
          case e: Exception =>
            l.logError(
              "could not parse quiz item with text " +
                s"$strPromptResponse using separator ${fromParams.separator.toString}"
            )
            None
        }.get

      def parseQuizItems(lineSplitter: StringSplitter): LazyList[QuizItem] =
        if lineSplitter.hasNext then
          parseQuizItem(lineSplitter.next) match {
            case Some(line) => LazyList.cons(line, parseQuizItems(lineSplitter))
            case _          => parseQuizItems(lineSplitter)
          }
        else LazyList.empty

      val lineSplitter = stringSplitterFactory.getSplitter('\n')
      lineSplitter.setString(text)
      val quizItems = parseQuizItems(lineSplitter)

      QuizGroupMemoryLevel(fromParams.index, fromParams.repetitionInterval, quizItems)
    }
  }

  implicit object customFormatDictionary extends CustomFormat[Dictionary, ParamsSeparator, ParamsNone] {
    /*
     * Currently, dictionaries are written by external scripts, and are read-only within
     * a running Libanius app, so this function is not implemented.
     */
    def to(component: Dictionary, strBuilder: StringBuilder, params: ParamsSeparator) =
      ???

    /*
     * Example:
     *
     * quizGroup type="WordMapping" promptType="English word" responseType="German word"
     *    against|wider
     *    entertain|unterhalten
     */
    def from(str: String, fromParams: ParamsNone): Dictionary =
      new Dictionary() {

        def parseCustomFormat() = {

          val splitterLineBreak = stringSplitterFactory.getSplitter('\n')
          val splitterKeyValue  = stringSplitterFactory.getSplitter('|')

          splitterLineBreak.setString(str)
          splitterLineBreak.next // skip the first line, which has already been parsed

          while splitterLineBreak.hasNext do {
            splitterKeyValue.setString(splitterLineBreak.next)

            if splitterKeyValue.hasNext then {
              val strKey = splitterKeyValue.next
              if splitterKeyValue.hasNext then {
                val strValues = splitterKeyValue.next
                // for efficiency, avoid an extra method call into Dictionary here
                wordMappings.put(strKey, WordMappingValueSetLazyProxy(strValues, "|"))
              }
            }
          }
        }

        Try(parseCustomFormat()) recover { case e: Exception =>
          l.logError(s"Could not parse dictionary: ${e.getMessage}", e)
          None
        }
      }
  }

  implicit object customFormatWordMappingGroup
      extends CustomFormat[WordMappingGroup, ParamsSeparator, ParamsSeparator]
      with AppDependencyAccess {

    /*
     * Not used, as WordMappingGroup is only used within Dictionary, which is not written to.
     */
    def to(component: WordMappingGroup, strBuilder: StringBuilder, params: ParamsSeparator) =
      ???

    /*
     * Example:
     *
     * quizGroup type="WordMapping" promptType="English word" responseType="German word" currentPromptNumber="0" isActive="true"
     *     against|wider
     *     entertain|unterhalten
     */
    def from(str: String, fromParams: ParamsSeparator): WordMappingGroup = {

      val splitterLineBreak   = stringSplitterFactory.getSplitter('\n')
      val wordMappingsMutable = new ListBuffer[WordMappingPair]()

      def parseQuizGroup(): Unit = {
        splitterLineBreak.setString(str)
        splitterLineBreak.next // skip the first line, which has already been parsed

        while splitterLineBreak.hasNext do {
          val strPromptResponse = splitterLineBreak.next

          def parsePromptResponse = {
            val i                      = strPromptResponse.indexOf(fromParams.separator)
            val strPrompt              = strPromptResponse.substring(0, i).trim
            val separatorLength        = fromParams.separator.length
            val strResponseAndUserInfo = strPromptResponse.substring(i + separatorLength)

            wordMappingsMutable += WordMappingPair(
              strPrompt,
              WordMappingValueSetLazyProxy(strResponseAndUserInfo, fromParams.separator)
            )
          }

          Try(parsePromptResponse) recover { case e: Exception =>
            l.logError(s"could not parse prompt-response string: $strPromptResponse")
          }
        }
      }
      Try(parseQuizGroup()) recover { case e: Exception =>
        l.logError(s"could not parse wmg with text ${str.take(100)}...${str.takeRight(100)})")
      }

      val wordMappingsStream = wordMappingsMutable.to(LazyList)

      // Now use the persistent data structure.
      new WordMappingGroup(QuizGroupHeader(str), wordMappingsStream, QuizGroupUserData(str))
    }
  }
}
