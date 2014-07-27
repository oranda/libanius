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

package com.oranda.libanius.model.quizgroup

import org.specs2.mutable.Specification

import com.oranda.libanius.model.TestData
import TestData._

class QuizGroupWithHeaderSpec extends Specification {

  "a quiz group with header" should {

    "be parseable from custom format" in {
      qgWithHeader.currentPromptNumber mustEqual 10
      qgWithHeader.promptType mustEqual "English word"
      qgWithHeader.responseType mustEqual "German word"

      qgWithHeader.levels(0).size mustEqual 8
      qgWithHeader.levels(1).size mustEqual 2
      qgWithHeader.levels(2).size mustEqual 2
    }
  }
}
