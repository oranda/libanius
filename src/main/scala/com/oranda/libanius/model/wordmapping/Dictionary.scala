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

package com.oranda.libanius.model.wordmapping

import scala.collection.JavaConverters._
import scala.collection.immutable.Stream

import com.oranda.libanius.model.{SearchResultPair, ModelComponent, SearchResult}
import com.oranda.libanius.model.quizgroup.{QuizGroup, QuizGroupHeader}
import com.oranda.libanius.model.quizitem.QuizItem

/**
 * A dictionary. A large read-only repository of word mappings, structured as a map
 * for fast access.
 */

// When populating, the java.util map is faster than the mutable Scala map
case class Dictionary(wordMappings: java.util.LinkedHashMap[String, WordMappingValueSet] =
    new java.util.LinkedHashMap[String, WordMappingValueSet]) extends ModelComponent {

  def numKeyWords = wordMappings.size

  def mappingsForKeysBeginningWith(keyStart: String): List[(String, WordMappingValueSet)] = {
    val matchingKeys = wordMappings.asScala.keys.filter(_.toLowerCase.startsWith(keyStart.toLowerCase))
    pairListForKeys(matchingKeys, 5)
  }

  def mappingsForKeysContaining(keyPart: String): List[(String, WordMappingValueSet)] = {
    val matchingKeys = wordMappings.asScala.keys.filter(_.toLowerCase.contains(keyPart.toLowerCase))
    pairListForKeys(matchingKeys, 5)
  }

  def pairListForKeys(keys: Iterable[String], size: Int):
      List[(String, WordMappingValueSet)] = {
    val keysSubset = keys.toList.sorted.slice(0, size)
    keysSubset.map(key => (key, wordMappings.get(key)))
  }

  def findValuesFor(keyWord: String): WordMappingValueSet = wordMappings.get(keyWord)
}


object Dictionary {

  def fromWordMappings(wordMappingsStream: Stream[WordMappingPair]) =
    new Dictionary() {
      wordMappingsStream.foreach(pair => wordMappings.put(pair.key, pair.valueSet))
    }

  def fromQuizItems(quizItems: Stream[QuizItem]) =
    new Dictionary() {
      val wMappingsStream = WordMappingGroup.quizItemsToWordMappingPairs(quizItems)
      wMappingsStream.foreach(pair => wordMappings.put(pair.key, pair.valueSet))
    }

  def fromQuizGroup(quizGroup: QuizGroup): Dictionary = fromQuizItems(quizGroup.quizItems)

  // Useful functions for search:

  def convertToSearchResults(pairs: List[(String, WordMappingValueSet)], header: QuizGroupHeader) =
    pairs.map {
      case (key, values) => SearchResult(header, SearchResultPair(key, values.toValueSet))
    }

  def searchFunction(param: => List[SearchResult]) = () => param

  def tryUntilResults(functionsToTry: List[() => List[SearchResult]]): List[SearchResult] =
    functionsToTry.find(_.apply.nonEmpty).map(_.apply).getOrElse(Nil)
}
