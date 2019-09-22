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

package com.oranda.libanius.simulation

import com.oranda.libanius.consoleui.Output._
import com.oranda.libanius.model.Quiz
import com.oranda.libanius.dependencies.AppDependencyAccess

/*
 * This is a larger test, loading a quiz of thousands of items from the filesystem.
 */
object FullQuiz extends App with Simulation with AppDependencyAccess {

  runQuiz()

  private def runQuiz(): Unit = {

    output("Running quiz...")

    val availableQuizGroupHeaders = dataStore.findAvailableQuizGroups
    if (availableQuizGroupHeaders.isEmpty)
      output("Tried to load quiz groups but none found!\n")
    else {
      val availableQuizGroups =
        List(availableQuizGroupHeaders.iterator.next)
          .map(header => (header, dataStore.loadQuizGroupCore(header))).toMap
      val quiz = Quiz(availableQuizGroups)
      testAllQuizItems(quiz)
    }
  }
}
