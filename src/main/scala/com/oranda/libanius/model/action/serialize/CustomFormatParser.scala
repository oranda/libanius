/*
 * Libanius
 * Copyright (C) 2012-2015 James McCabe <james@oranda.com>
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

import com.oranda.libanius.model._
import com.oranda.libanius.model.quizgroup._
import com.oranda.libanius.model.quizitem._
import com.oranda.libanius.model.wordmapping.{WordMappingValueSet, WordMappingValue}
import com.oranda.libanius.util.StringUtil

import scala.collection.immutable.{List, Stream}
import scala.language.postfixOps
import scala.util.parsing.combinator._
import scala.util.parsing.input.Reader

object CustomFormatParser extends JavaTokenParsers {

  // skipWhitespace remains true but whiteSpace is overriden so that it doesn't include newline.
  override val whiteSpace = """[ \t]*""".r

  private val eol = sys.props("line.separator")

  def deserialize(qgwhText: Reader[Char]): QuizGroupWithHeader =
    parseQuizGroupWithHeader(qgwhText)

  protected[serialize] def userResponse: Parser[UserResponse] =
    """\d+""".r ^^ { case x => UserResponse(x.toInt) }

  // Example: 9,7
  protected[serialize] def userResponses: Parser[List[UserResponse]] =
    repsep(userResponse, ",")

  // Example: 1,7,9;6
  protected[serialize] def userResponsesAll: Parser[UserResponsesAll] =
    ((userResponses~(";"~>userResponses))?) ^^ {
      case Some(correct~incorrect) => UserResponsesAll(correct, incorrect)
      case _ => UserResponsesAll(Nil, Nil)
    }

  def freeText: Parser[String] = """[^|]*""".r ^^ { _.toString }
  def prompt: Parser[String] = """[^|#]*""".r ^^ { _.toString }
  def response: Parser[String] = freeText

  // Example: "nachlösen|9,7;6"
  protected[serialize] def wordMappingValue(implicit sep: Separator): Parser[WordMappingValue] =
    freeText~(sep.toString~>userResponsesAll) ^^ {
      case (text~userResponsesAll) => WordMappingValue(text, userResponsesAll)
    }

  def parseWordMappingValueSet(wmvsText: Reader[Char])(implicit sep: Separator = Separator("|")):
      WordMappingValueSet =
    parse(wordMappingValueSet, wmvsText) match {
      case Success(matched, _) => WordMappingValueSet(matched)
      case Failure(msg, _) => println("FAILURE: " + msg); WordMappingValueSet()
      case Error(msg, _) => println("ERROR: " + msg); WordMappingValueSet()
    }

  // Example: "contract|698,696;697/treaty|796;798"
  protected[serialize] def wordMappingValueSet(implicit sep: Separator):
      Parser[List[WordMappingValue]] =
    repsep(wordMappingValue, "/")


  protected[serialize] def promptAndResponse(implicit sep: Separator):
      Parser[(TextValue, TextValue)] =
    prompt~(sep.toString~>response) ^^ {
      case (prompt~correctResponse) => (TextValue(prompt.trim), TextValue(correctResponse.trim))
    }

  def parseQuizItem(text: Reader[Char])(implicit sep: Separator): QuizItem =
    parse(quizItem, text) match {
      case Success(matched, _) => matched
      case Failure(msg, _) => println("FAILURE: " + msg); QuizItem("", "")
      case Error(msg, _) => println("ERROR: " + msg); QuizItem("", "")
    }

  // Example: Vertrag|treaty|796;798
  protected[serialize] def quizItem(implicit sep: Separator): Parser[QuizItem] =
    promptAndResponse~(sep.toString~>userResponsesAll).? ^^ {
      case (promptAndResponse~userResponsesAll) =>
        QuizItemConcrete(
            promptAndResponse._1,
            promptAndResponse._2,
            userResponsesAll.getOrElse(UserResponsesAll()))
    }

  def quotedInt: Parser[String] = ("\""+"""\d+""" +"\"").r
  def dequotedInt: Parser[Int] = quotedInt ^^ { str => str.substring(1, str.length - 1).toInt }
  def dequotedString: Parser[String] = stringLiteral ^^ { str => str.substring(1, str.length - 1) }

  def parseNvpInt(text: String, name: String): Int =
    parse(nvpInt(name), text) match {
      case Success(matched, _) => matched
      case Failure(msg, _) => println("FAILURE: " + msg); -1
      case Error(msg, _) => println("ERROR: " + msg); -1
    }

  def nvpString(name: String): Parser[String] = (name+"=")~>dequotedString
  def nvpInt(name: String): Parser[Int] = (name+"=")~>dequotedInt
  def nvpBoolean(name: String): Parser[Boolean] = nvpString(name) ^^ { _.toBoolean }

  def parseQuizGroupMemoryLevelHeader(mlHeader: Reader[Char]): (Int, Int) =
    parse(quizGroupMemoryLevelHeader, mlHeader) match {
      case Success(matched, _) => matched
      case Failure(msg, _) => println("FAILURE: " + msg); (0, 0)
      case Error(msg, _) => println("ERROR: " + msg); (0, 0)
    }

  // Example: #quizGroupPartition numCorrectResponsesInARow="0" repetitionInterval="0"
  protected[serialize] def quizGroupMemoryLevelHeader: Parser[(Int, Int)] =
    "#quizGroupPartition"~>nvpInt("numCorrectResponsesInARow")~nvpInt("repetitionInterval") ^^ {
      case (numCorrectResponses~repetitionInterval) => (numCorrectResponses, repetitionInterval)
    }


  def quizGroupMemoryLevelBody(implicit sep: Separator): Parser[List[QuizItem]] =
    repsep(quizItem, eol)

  protected[serialize] def quizGroupMemoryLevel(implicit sep: Separator):
      Parser[QuizGroupMemoryLevel] =
    quizGroupMemoryLevelHeader~(eol~>quizGroupMemoryLevelBody) ^^ {
      case (qgmlHeader~qgmlBody) => QuizGroupMemoryLevel(
          correctResponsesInARow = qgmlHeader._1,
          repetitionInterval = qgmlHeader._2,
          quizItemStream = qgmlBody.toStream,
          totalResponses = 0,
          numCorrectResponses = 0)
    }

  protected[serialize] def quizGroupBody(implicit sep: Separator):
      Parser[Map[Int, QuizGroupMemoryLevel]] =
    repsep(quizGroupMemoryLevel, eol) ^^ {
      case (qgmls: List[QuizGroupMemoryLevel]) =>
        val levelsMap: Map[Int, QuizGroupMemoryLevel] =
          qgmls.map(qgml => (qgml.correctResponsesInARow, qgml)).toMap
        levelsMap
  }

  protected[serialize] def qgType: Parser[QuizGroupType] = nvpString("type") ^^ {
    case "WordMapping" => WordMapping
    case "QuestionAndAnswer" => QuestionAndAnswer
  }

  def parseQuizGroupHeader(qghText: Reader[Char]): QuizGroupHeader = {

    val defaultQuizGroupHeader = QuizGroupHeader(WordMapping, "", "", "|", 4)

    parse(quizGroupHeader, qghText) match {
      case Success(matched, _) => matched
      case Failure(msg, _) => println("FAILURE: " + msg); defaultQuizGroupHeader
      case Error(msg, _) => println("ERROR: " + msg); defaultQuizGroupHeader
    }
  }

  // Example: #quizGroup type="WordMapping" promptType="English word" responseType="German word" mainSeparator="|" useMultipleChoiceUntil="4" currentPromptNumber="0" isActive="true"
  protected[serialize] def quizGroupHeader: Parser[QuizGroupHeader] =
    "#quizGroup"~>qgType~nvpString("promptType")~nvpString("responseType")~(nvpString("mainSeparator").?)~(nvpInt("useMultipleChoiceUntil").?) ^^ {
      case (qgType~promptType~responseType~mainSeparator~useMultipleChoiceUntil) =>
        QuizGroupHeader(
          quizGroupType = qgType,
          promptType = promptType,
          responseType = responseType,
          mainSeparator = Separator(mainSeparator.getOrElse("|")),
          useMultipleChoiceUntil = useMultipleChoiceUntil.getOrElse(4))
    }

  def parseQuizGroupUserData(qgwhText: Reader[Char]): QuizGroupUserData =
    parse(quizGroupUserData, qgwhText) match {
      case Success(matched, _) => matched
      case Failure(msg, _) => println("FAILURE: " + msg); QuizGroupUserData()
      case Error(msg, _) => println("ERROR: " + msg); QuizGroupUserData()
    }

  // Example:  currentPromptNumber="10" isActive="true"
  protected[serialize] def quizGroupUserData: Parser[QuizGroupUserData] =
    nvpBoolean("isActive")~nvpInt("currentPromptNumber") ^^ {
      case (isActive~currentPromptNumber) =>
        QuizGroupUserData(isActive, currentPromptNumber)
    }

  def parseQuizGroupHeaderAndUserData(qgwhText: Reader[Char]): (QuizGroupHeader, QuizGroupUserData) = {

    val defaultQuizGroupHeader = QuizGroupHeader(WordMapping, "", "", "|", 4)
    val emptyQghaud = (defaultQuizGroupHeader, QuizGroupUserData())

    parse(quizGroupHeaderAndUserData, qgwhText) match {
      case Success(qghaud, _) => qghaud
      case Failure(msg, _) => println("FAILURE: " + msg); emptyQghaud
      case Error(msg, _) => println("ERROR: "+ msg); emptyQghaud
    }
  }


  protected[serialize] def quizGroupHeaderAndUserData:
      Parser[(QuizGroupHeader, QuizGroupUserData)] =
    quizGroupHeader~quizGroupUserData ^^ { case(qgHeader~qgud) => (qgHeader, qgud) }

  // There are two passes over the quizGroupHeader due to context-sensitivity
  def parseQuizGroupWithHeader(qgwhText: Reader[Char]): QuizGroupWithHeader =
    parse(quizGroupHeaderAndUserData, qgwhText) match {
      case Success(qghaud, _) => parseQuizGroupWithHeaderUsingSeparator(qgwhText, qghaud._1.mainSeparator)
      case Failure(msg, _) => println("FAILURE: " + msg); QuizGroupWithHeader(QuizGroupHeader(""), QuizGroup())
      case Error(msg, _) => println("ERROR: "+ msg); QuizGroupWithHeader(QuizGroupHeader(""), QuizGroup())
    }

  def parseQuizGroupWithHeaderUsingSeparator(qgwhText: Reader[Char], separator: Separator):
      QuizGroupWithHeader =
    parse(quizGroupWithHeader(separator), qgwhText) match {
      case Success(matched, _) => matched
      case Failure(msg, _) => println("FAILURE: " + msg); QuizGroupWithHeader(QuizGroupHeader(""), QuizGroup())
      case Error(msg, _) => println("ERROR: " + msg); QuizGroupWithHeader(QuizGroupHeader(""), QuizGroup())
    }

  protected[serialize] def quizGroupWithHeader(implicit separator: Separator):
      Parser[QuizGroupWithHeader] =
    quizGroupHeaderAndUserData~(eol~>quizGroupBody) ^^ {
      case((qgh, qgud)~qgBody) => QuizGroupWithHeader(qgh, QuizGroup(qgBody, qgud))
    }

}
