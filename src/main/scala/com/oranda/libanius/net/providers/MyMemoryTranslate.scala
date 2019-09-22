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

package com.oranda.libanius.net.providers

import java.net.URLEncoder
import play.api.libs.functional.syntax._
import play.api.libs.json._

import scala.language.implicitConversions
import com.oranda.libanius.net.Rest
import com.oranda.libanius.dependencies.AppDependencyAccess
import com.oranda.libanius.model._
import com.oranda.libanius.model.quizgroup.QuizGroupHeader
import scala.util.Try
import com.oranda.libanius.model.ValueSet
import com.oranda.libanius.model.SearchResultPair
import com.oranda.libanius.model.SearchResult
import com.oranda.libanius.util.CollectionHelpers.GroupByOrderedImplicit

/*
 * Use the free online service mymemory.translated.net to translate strings.
 */
object MyMemoryTranslate extends AppDependencyAccess {

  def translate(word: String, quiz: Quiz): Try[List[SearchResult]] = {
    val tryTranslate = Try(quiz.activeQuizGroupHeaders.flatMap(translateQgh(word, _)).toList)
    tryTranslate.recover {
      case t: Throwable => l.logError("Unknown error in MyMemoryTranslate", t)
    }
    // Rethrow, so the client can decide on a custom error message.
    tryTranslate
  }


  protected[providers] def translateQgh(word: String, header: QuizGroupHeader):
      List[SearchResult] = {

    val matches: List[(String, String)] =
      mmCode(header).toList.flatMap(translateOrError(word, _))

    // Group by key so as to go from (key, value) to (key, values)
    val groupedMatches = matches.groupByOrdered(_._1).map {
      case (key, valueSet) =>
        SearchResult(header, SearchResultPair(key, ValueSet(valueSet.map(_._2).toList)))
    }
    groupedMatches.filterNot(_.keyWordMatchesValue).toList
  }

  private[this] def urlEncode(str: String) = URLEncoder.encode(str, "UTF-8")

  /*
   * This method is prone to throwing exceptions. As suggested by the name, it should be guarded.
   */
  private[this] def translateOrError(word: String, mmCode: String): List[(String, String)] = {
    val queryArgs = s"q=${urlEncode(word)}&de=${conf.email}&langpair=${urlEncode(mmCode)}"
    val restQuery = "http://api.mymemory.translated.net/get?" + queryArgs
    val translationRaw = Rest.query(restQuery)
    val matches = Try(findMatchesInJson(translationRaw)).recover {
      case t: Throwable =>
        l.logError(s"Could not parse JSON text: $translationRaw", t)
        List[TranslationMatch]()
    }.get
    // Filter on the quality of the match.
    matches.filter(_.matchWeight >= 0.5).map(trMatch => (trMatch.segment, trMatch.translation))
  }


  case class TranslationMatch(segment: String, translation: String, matchWeight: BigDecimal)

  def findMatchesInJson(jsonRaw: String): List[TranslationMatch] = {

    implicit val matchesReader = (
      (__ \ "segment").read[String] and
      (__ \ "translation").read[String] and
      (__ \ "match").read[BigDecimal]
    )(TranslationMatch)

    val translationJson: JsValue = Json.parse(jsonRaw)
    val matches = (translationJson \ "matches").as[List[TranslationMatch]]
    matches.map(m => m.copy(translation = m.translation.replaceAll("""[\p{Punct}]""", "")))
  }


  /*
   * Get the MyMemory string corresponding to the QuizGroupHeader, e.g. "ger|en", or
   * None if there is none known.
   */
  private[this] def mmCode(header: QuizGroupHeader): Option[String] =
    mmCode(header.promptType, header.responseType)

  val mmCode: PartialFunction[String, String] = {
    case "German word" => "ger"
    case "English word" => "en"
    case "Spanish word" => "spa"
  }

  private[this] def mmCode(promptType: String, responseType: String): Option[String] =
    if (mmCode.isDefinedAt(promptType) && mmCode.isDefinedAt(responseType))
      Some(mmCode.apply(promptType) + "|" + mmCode.apply(responseType))
    else
      None
}
