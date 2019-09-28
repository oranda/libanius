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
import com.oranda.libanius.model._
import com.oranda.libanius.model.quizgroup._
import com.oranda.libanius.model.quizitem.TextValueOps.TextValue
import com.oranda.libanius.model.quizitem.{QuizItemConcrete, QuizItem}
import com.oranda.libanius.model.wordmapping.{WordMappingValueSet, WordMappingValue}
import com.oranda.libanius.util.StringSplitter
import fastparse.all._
import fastparse.core.Parsed

import scala.collection.immutable.Stream

object CustomFormatParserFast extends AppDependencyAccess {

  // Example input: 9
  protected[serialize] val userResponse: P[Option[UserResponse]] =
    P ( CharIn('0'to'9').rep.! ).map (s => if (s.isEmpty) None else Option(UserResponse(s.toInt)))

  // Example input: 9,7
  protected[serialize] val userResponses: P[List[UserResponse]] =
    P( userResponse.rep(sep = ",").map(_.flatten.toList) )

  // Example input: 1,7,9;6
  protected[serialize] def userResponsesAll: P[UserResponsesAll] =
    P( userResponses ~ (";".? ~ userResponses) ).map {
      case (correctResponsesInARow, incorrectResponses) =>
        UserResponsesAll(correctResponsesInARow, incorrectResponses)
    }

  // An optimized but verbose version of userResponsesAll
  protected[serialize] def userResponsesAllFast: P[UserResponsesAll] =
    P( CharPred(c => c != '\n' && c != '/').rep.! ).map { case ura =>
      def toUserResponses(responseList: Option[String]) =
        responseList.map(responses =>
          if (responses.isEmpty) Nil
          else responses.split(',').map(d => UserResponse(d.toInt)).toList
        ).getOrElse(Nil)

      val responseLists = ura.split(";")
      val correctResponses = toUserResponses(responseLists.headOption)
      val incorrectResponses = toUserResponses(responseLists.lift(1))
      UserResponsesAll(correctResponses, incorrectResponses)
    }

  protected[serialize] def freeText(implicit sep: Separator): P[String] = {
    val septxt = sep.mainSeparator
    // For efficiency, the code is different if the separator is a single character
    if (sep.mainSeparator.length == 1) P( CharsWhile(_.!=(sep.mainSeparator.charAt(0))).! )
    else P( (!septxt ~ AnyChar).rep.! )
  }

  protected[serialize] def prompt(implicit sep: Separator): P[String] = freeText
  protected[serialize] def response(implicit sep: Separator): P[String] = freeText

  // Example input: "nachlösen|9,7;6"
  protected[serialize] def wordMappingValue(implicit sep: Separator): P[WordMappingValue] =
    P( freeText ~ sep.toString ~ userResponsesAllFast ).map {
      case (text, userResponsesAll) => WordMappingValue(text, userResponsesAll)
    }

  // Example input: "contract|698,696;697/treaty|796;798"
  def wordMappingValueSet(implicit sep: Separator): P[WordMappingValueSet] =
    P( wordMappingValue.rep(sep = "/") ).map(wmvs => WordMappingValueSet(wmvs.toList))

  protected[serialize] def promptAndResponse(implicit sep: Separator): P[(TextValue, TextValue)] =
    P( prompt ~ sep.toString ~ response ).map {
      case (prompt, correctResponse) => (prompt.trim, correctResponse.trim)
    }

  // Example input: Vertrag|treaty|796;798
  protected[serialize] def quizItem(implicit sep: Separator): P[QuizItem] =
    P( promptAndResponse ~ (sep.toString ~ userResponsesAllFast).? ).map {
      case (prompt, response, userResponsesAll) =>
        QuizItemConcrete(prompt, response, userResponsesAll.getOrElse(UserResponsesAll()))
    }

  protected[serialize] def dequotedString: P[String] =
    P( "\"" ~ CharsWhile(_.!=('\"')).! ~ AnyChar)
  protected[serialize] def quotedInt: P[String] =
    P( ("\"" ~ CharPred(_.isDigit).rep ~ "\"").! )
  protected[serialize] def dequotedInt: P[Int] =
    P( "\"" ~ CharPred(_.isDigit).rep.! ~ "\"" ).map(_.toInt)

  protected[serialize] def nvpString(name: String): P[String] =
    P( " ".rep ~ name ~ "=" ~ dequotedString ~ " ".rep)
  protected[serialize] def nvpInt(name: String): P[Int] =
    P( nvpString(name).map(_.toInt))
  protected[serialize] def nvpBoolean(name: String): P[Boolean] =
    P( nvpString(name).map(_.toBoolean))

  // Example input: #quizGroupPartition numCorrectResponsesInARow="0" repetitionInterval="0"
  protected[serialize] def quizGroupMemoryLevelHeader: P[(Int, Int)] =
    P( "#quizGroupPartition " ~ nvpInt("numCorrectResponsesInARow") ~
        nvpInt("repetitionInterval").?).map {
      case (numCorrectResponses, repetitionInterval) =>
          (numCorrectResponses, repetitionInterval.getOrElse(0))
    }

  // Not using normal parser combinator semantics: return a stream of quiz items for efficiency
  protected[serialize] def quizGroupMemoryLevelBody(implicit sep: Separator): P[Stream[QuizItem]] =
    P ( CharsWhile(c => c != '#').! ).map { case text =>
      val quizItemParser = quizItem // parser instantiation is expensive, so only do it once
      def parseQuizItems(lineSplitter: StringSplitter): Stream[QuizItem] = {
        if (lineSplitter.hasNext) {
          val quizItemText = lineSplitter.next
          quizItemParser.parse(quizItemText) match {
            case Parsed.Success(qi, _) =>
              Stream.cons(qi, parseQuizItems(lineSplitter))
            case _ => // shouldn't happen
              l.logError(s"Could not parse quiz item $quizItemText")
              parseQuizItems(lineSplitter) // continue with the next
          }
        } else
          Stream.empty
      }
      val lineSplitter = stringSplitterFactory.getSplitter('\n')
      lineSplitter.setString(text)
      parseQuizItems(lineSplitter)
    }

  protected[serialize] def quizGroupMemoryLevel(implicit sep: Separator): P[QuizGroupMemoryLevel] =
    P( quizGroupMemoryLevelHeader ~ "\n" ~ quizGroupMemoryLevelBody).map {
      case (responsesInARow, repInterval, qgmlBody) =>
        QuizGroupMemoryLevel(
            correctResponsesInARow = responsesInARow,
            repetitionInterval = repInterval,
          quizItemStream = qgmlBody,
            totalResponses = 0,
            numCorrectResponses = 0)
    }

  // Example input: isActive="true" currentPromptNumber="10"
  def quizGroupUserData: P[QuizGroupUserData] =
    P( nvpBoolean("isActive") ~ nvpInt("currentPromptNumber") ).map {
      case (isActive, currentPromptNumber) => QuizGroupUserData(isActive, currentPromptNumber)
    }

  protected[serialize] def qgType: Parser[QuizGroupType] =
    nvpString("type").map(QuizGroupType.fromString)

  // Example input:
  // #quizGroup type="WordMapping" promptType="English word" responseType="German word" mainSeparator="|" useMultipleChoiceUntil="4" currentPromptNumber="0" isActive="true"
  def quizGroupHeader: Parser[QuizGroupHeader] =
    P( "#quizGroup" ~ qgType ~ nvpString("promptType") ~ nvpString("responseType") ~
        nvpString("mainSeparator").? ~ nvpInt("useMultipleChoiceUntil").?).map {
      case (qgType, promptType, responseType, mainSeparator, useMultipleChoiceUntil) =>
        QuizGroupHeader(
          quizGroupType = qgType,
          promptType = promptType,
          responseType = responseType,
          mainSeparator = Separator(mainSeparator.getOrElse("|")),
          useMultipleChoiceUntil = useMultipleChoiceUntil.getOrElse(4))
    }

  protected[serialize] def quizGroupHeaderAndUserData: P[(QuizGroupHeader, QuizGroupUserData)] =
    P( quizGroupHeader ~ quizGroupUserData ).map { case(qgHeader, qgud) => (qgHeader, qgud) }

  type QuizGroupBody = Map[Int, QuizGroupMemoryLevel]

  protected[serialize] def quizGroupBody(implicit sep: Separator): P[QuizGroupBody] =
    P( quizGroupMemoryLevel.rep ).map {
      case (qgmls: Seq[QuizGroupMemoryLevel]) =>
        qgmls.map(qgml => (qgml.correctResponsesInARow, qgml)).toMap
    }

  // Entry point for this parser.
  def quizGroupWithHeader: P[QuizGroupWithHeader] = {

    def quizGroupWithHeaderInner(quizGroupHeader: QuizGroupHeader, qgud: QuizGroupUserData):
        P[QuizGroupWithHeader] =
      P (quizGroupBody(quizGroupHeader.mainSeparator)).map {
        case (qgBody) => QuizGroupWithHeader(quizGroupHeader, QuizGroup(qgBody, qgud))
      }

    P( quizGroupHeaderAndUserData ).flatMap {
      // If the header specifies a separator, it is used in the parsing of the quiz group body.
      case (quizGroupHeader, qgud) => "\n" ~ quizGroupWithHeaderInner(quizGroupHeader, qgud)
    }
  }

}
