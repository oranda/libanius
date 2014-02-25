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

package com.oranda.libanius.net.providers

import org.specs2.mutable.Specification
import com.oranda.libanius.dependencies.AppDependencyAccess
import com.oranda.libanius.model.SearchResult
import com.oranda.libanius.model.quizgroup.{WordMapping, QuizGroupHeader}


class MyMemoryTranslateSpec extends Specification with AppDependencyAccess {

  "the mymemory translation provider" should {

    "translate a word successfully" in {
      val qgHeader = QuizGroupHeader(WordMapping, "German word", "English word", "|")
      val results: List[SearchResult] = MyMemoryTranslate.translateQgh("Bett", qgHeader)
      results.head.valueSet.containsValue("Bed") mustEqual true
    }

    "return an empty list if there is a problem translating a word" in {
      val qgHeader = QuizGroupHeader(WordMapping, "German word", "English word", "|")
      val results: List[SearchResult] = MyMemoryTranslate.translateQgh("abcdef", qgHeader)
      results.isEmpty mustEqual true
    }
  }
}