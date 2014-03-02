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

import org.specs2.mutable.Specification
import com.oranda.libanius.dependencies.AppDependencyAccess
import scala.collection.immutable.ListMap

class StatsAllMemoryLevelsSpec extends Specification with AppDependencyAccess {

  "the statistics on all memory levels" should {

    val statsLevel2 = StatsMemoryLevel(totalResponses = 2, numCorrectResponsesInARow = 1)
    val allStats = StatsAllMemoryLevels(stats = ListMap(2 -> statsLevel2))

    "be updated for a correct answer" in {
      val newAllStats = allStats.incrementResponsesCorrect(2)
      newAllStats.totalResponses(2) mustEqual 3
      newAllStats.numCorrectResponsesInARow(2) mustEqual 2
    }

    "be updated for a incorrect answer" in {
      val newAllStats = allStats.incrementResponsesIncorrect(2)
      newAllStats.totalResponses(2) mustEqual 3
      newAllStats.numCorrectResponsesInARow(2) mustEqual 1
    }

    "return None on an attempt to get information from an unknown memory level" in {
      allStats.totalResponses(0) mustEqual 0
    }

    "be updated correctly for an unknown memory level" in {
      val newAllStats = allStats.incrementResponsesCorrect(0)
      newAllStats.totalResponses(0) mustEqual 1
      newAllStats.numCorrectResponsesInARow(0) mustEqual 1
    }

    "have a count reset for a level" in {
      val statsLevel2 = StatsMemoryLevel(totalResponses = 10, numCorrectResponsesInARow = 1)
      val allStats = StatsAllMemoryLevels(stats = ListMap(2 -> statsLevel2))
      val newAllStats = allStats.incrementResponsesCorrect(2)
      newAllStats.totalResponses(2) mustEqual 1
      newAllStats.numCorrectResponsesInARow(2) mustEqual 1
    }
  }
}
