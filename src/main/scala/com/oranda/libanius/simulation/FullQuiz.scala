/*
 * Copyright 2012-2013 James McCabe <james@oranda.com>
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may
 * not use this file except in compliance with the License. You may obtain
 * a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
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

  private def runQuiz() {

    output("Running quiz...")

    val availableQuizGroupHeaders = dataStore.findAvailableQuizGroups
    if (availableQuizGroupHeaders.isEmpty)
      output("Tried to load quiz groups but none found!\n")
    else {
      val availableQuizGroups = List(availableQuizGroupHeaders.iterator.next).map(
          header => (header, dataStore.loadQuizGroupCore(header))).toMap
      val quiz = Quiz(availableQuizGroups)
      testAllQuizItems(quiz)
    }
  }
}
