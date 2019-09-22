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

import com.oranda.libanius.model.quizgroup.QuizGroupHeader

case class SearchResult(quizGroupHeader: QuizGroupHeader, wmp: SearchResultPair) {
  lazy val promptType = quizGroupHeader.promptType
  lazy val responseType = quizGroupHeader.responseType
  lazy val keyWord = wmp.key
  lazy val valueSet = wmp.valueSet

  // It may be desired to filter out trivial search results where this is true
  def keyWordMatchesValue = valueSet.size == 1 && keyWord == valueSet.values.head

  override def toString = s"$keyWord: ${valueSet.toString}"
}
