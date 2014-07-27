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
import com.oranda.libanius.model.quizitem.QuizItem
import com.oranda.libanius.model.wordmapping.{WordMappingValueSet, WordMappingValue}
import com.oranda.libanius.util.StringUtil

/**
 * Type class definition for ModelComponent serialization/deserialization using a custom format.
 *
 * TODO: get rid of the underscores.
 */
trait CustomFormat[A <: ModelComponent, B <: Params] extends /*FromCustomFormat[A] with*/ ToCustomFormat[A, B]
//trait CustomFormatWithSeparator[A <: ModelComponent] extends /*FromCustomFormat[A] with*/ ToCustomFormatWithSeparator[A]


/*
trait FromCustomFormat[A <: ModelComponent] {
  def fromCustomFormat(str: String, mainSeparator: String): A
}
*/
trait ToCustomFormat[A <: ModelComponent, B <: Params] {
  def to(component: A, params: B): StringBuilder
}

//abstract class FormattingParams(val strBuilder: StringBuilder)

trait Params {
  val strBuilder: StringBuilder
}

case class ParamsDefault(override val strBuilder: StringBuilder) extends Params {
  def withSeparator(separator: String) = ParamsWithSeparator(strBuilder, separator)
}

case class ParamsWithSeparator(override val strBuilder: StringBuilder,
    mainSeparator: String) extends Params {
  def withIndex(index: Int) = ParamsWithSeparatorAndIndex(strBuilder,
    mainSeparator, index)
}

case class ParamsWithSeparatorAndIndex(override val strBuilder: StringBuilder,
    mainSeparator: String, index: Int) extends Params {
  def withoutIndex = ParamsWithSeparator(strBuilder, mainSeparator)
}

/*
trait ToCustomFormatWithSeparator[A <: ModelComponent] {
  def _toCustomFormat(strBuilder: StringBuilder, component: A)
      (implicit mainSeparator: String): StringBuilder
}*/


// provides external access to the typeclass, forwarding the call to the appropriate type
object CustomFormat {
  /*
  def fromCustomFormat[A <: ModelComponent](str: String, mainSeparator: String)
      (implicit customFormat: CustomFormat[A]) =
    customFormat.fromCustomFormat(str, mainSeparator)
  */
  def to[A <: ModelComponent, B <: Params](component: A, params: B)
      (implicit customFormat: CustomFormat[A, B]): StringBuilder = {
    customFormat.to(component, params)
  }
}

object CustomFormatForModelComponents {
  implicit object toCustomFormatQuizGroupHeader
      extends CustomFormat[QuizGroupHeader, ParamsDefault] {
    // def _fromCustomFormat = TODO
    def to(qgh: QuizGroupHeader, params: ParamsDefault) =
      params.strBuilder.append("#quizGroup type=\"").append(qgh.quizGroupType).
          append("\" promptType=\""). append(qgh.promptType).
          append("\" responseType=\"").append(qgh.responseType).
          append("\" mainSeparator=\"").append(qgh.mainSeparator).
          append("\" useMultipleChoiceUntil=\"").append(qgh.useMultipleChoiceUntil).
          append("\"")
  }

  implicit object toCustomFormatQuizGroupUserData
      extends CustomFormat[QuizGroupUserData, ParamsDefault] {
    // def _fromCustomFormat = TODO
    def to(qgud: QuizGroupUserData, params: ParamsDefault) =
      params.strBuilder.append(" currentPromptNumber=\"").
          append(qgud.currentPromptNumber).append("\"").append(" isActive=\"").
          append(qgud.isActive).append("\"")
  }

  implicit object toCustomFormatQuizGroupWithHeader
      extends CustomFormat[QuizGroupWithHeader, ParamsDefault] {
    /*
     * Example of custom format:
     *
     * quizGroup type="WordMapping" promptType="English word" responseType="German word" currentPromptNumber="0" isActive="true"
     *    against|wider
     *    entertain|unterhalten
     */
    def to(qgwh: QuizGroupWithHeader, params: ParamsDefault): StringBuilder = {
      toCustomFormatQuizGroupHeader.to(qgwh.header, params)
      toCustomFormatQuizGroupUserData.to(qgwh.quizGroup.userData, params)
      params.strBuilder.append('\n')
      toCustomFormatQuizGroup.to(qgwh.quizGroup,
          params.withSeparator(qgwh.header.mainSeparator))
    }
  }

  implicit object toCustomFormatQuizItem extends CustomFormat[QuizItem, ParamsWithSeparator] {
    def to(qi: QuizItem, params: ParamsWithSeparator): StringBuilder = {
      params.strBuilder.append(qi.prompt).append(params.mainSeparator).
          append(qi.correctResponse).append(params.mainSeparator)
      toCustomFormatUserResponses.to(qi.userResponses, params)
    }
  }

  // Example: contract:696,697;698/treaty:796;798
  implicit object toCustomFormatWordMappingValueSet
      extends CustomFormat[WordMappingValueSet, ParamsWithSeparator] {
    def to(wmvs: WordMappingValueSet, params: ParamsWithSeparator): StringBuilder = {
      def wmvToCustomFormat(strBuilder: StringBuilder, wmv: WordMappingValue):
          StringBuilder =
        toCustomFormatWordMappingValue.to(wmv, params)

      StringUtil.mkString(params.strBuilder, wmvs.values, wmvToCustomFormat, '/')
    }
  }

  // Example: nachlösen:1,7,9;6
  implicit object toCustomFormatWordMappingValue
      extends CustomFormat[WordMappingValue, ParamsWithSeparator] {
    def to(wmv: WordMappingValue, params: ParamsWithSeparator): StringBuilder = {
      params.strBuilder.append(wmv.value)

      if (!wmv.correctAnswersInARow.isEmpty || !wmv.incorrectAnswers.isEmpty)
        params.strBuilder.append(params.mainSeparator)
      if (!wmv.correctAnswersInARow.isEmpty)
        StringUtil.mkString(params.strBuilder, wmv.correctAnswersInARow,
            wmv.answerPromptNumber, ',')
      if (!wmv.incorrectAnswers.isEmpty) {
        params.strBuilder.append(';')
        StringUtil.mkString(params.strBuilder, wmv.incorrectAnswers, wmv.answerPromptNumber, ',')
      }
      params.strBuilder
    }
  }

  // Example: 1,7,9;6
  implicit object toCustomFormatUserResponses
      extends CustomFormat[UserResponses, ParamsWithSeparator] {
    def to(ur: UserResponses, params: ParamsWithSeparator): StringBuilder = {
      if (!ur.correctResponsesInARow.isEmpty)
        StringUtil.mkString(params.strBuilder, ur.correctResponsesInARow,
          ur.responsePromptNumber, ',')
      if (!ur.incorrectResponses.isEmpty) {
        params.strBuilder.append(';')
        StringUtil.mkString(params.strBuilder, ur.incorrectResponses,
          ur.responsePromptNumber, ',')
      }
      params.strBuilder
    }
  }

  implicit object toCustomFormatQuizGroup
      extends CustomFormat[QuizGroup, ParamsWithSeparator] {
    def to(qg: QuizGroup, params: ParamsWithSeparator): StringBuilder = {
      for (memLevel <- qg.levels.zipWithIndex.toStream)
        toCustomFormatQuizGroupMemoryLevel.to(memLevel._1,
            params.withIndex(memLevel._2))
      params.strBuilder
    }
  }

  implicit object toCustomFormatQuizGroupMemoryLevel
      extends CustomFormat[QuizGroupMemoryLevel, ParamsWithSeparatorAndIndex] {
    def to(qgml: QuizGroupMemoryLevel, params: ParamsWithSeparatorAndIndex) = {
      params.strBuilder.append("#quizGroupPartition numCorrectResponsesInARow=\"" +
          params.index + "\" repetitionInterval=\"" + qgml.repetitionInterval + "\"" + '\n')

      for (quizItem <- qgml.quizItems.toStream) {
        toCustomFormatQuizItem.to(quizItem, params.withoutIndex)
        params.strBuilder.append('\n')
      }
      params.strBuilder
    }
  }
}