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

package com.oranda.libanius.net.providers

import com.oranda.libanius.dependencies.AppDependencyAccess
import com.oranda.libanius.model.SearchResult
import com.oranda.libanius.model.quizgroup.QuizGroupHeader
import com.oranda.libanius.model.quizgroup.QuizGroupType.WordMapping
import org.specs2.mutable.Specification

class MyMemoryTranslateSpec extends Specification with AppDependencyAccess {
  "the mymemory translation provider" should {
    val qghGerEng = QuizGroupHeader("German word", "English word", WordMapping, "|", 6, 4)

    "translate a word successfully" in {
      val results: List[SearchResult] = MyMemoryTranslate.translateQgh("Bett", qghGerEng)
      results.nonEmpty must beTrue
      results.head.valueSet.containsValue("Bed") must beTrue
    }

    "return an empty list if there is a problem translating a word" in {
      val results: List[SearchResult] = MyMemoryTranslate.translateQgh("xyz5abcdef", qghGerEng)
      results.isEmpty must beTrue
    }
  }
}
