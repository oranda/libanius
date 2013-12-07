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

package com.oranda.libanius.consoleui

import Output._
import com.oranda.libanius.model._

object DemoQuiz extends InteractiveQuiz {

  runQuiz()

  def runQuiz() {

    output("Running demo quiz...")
    val quiz = Quiz.demoQuiz()
    output("OK, the quiz begins! To quit, type q at any time.\n")
    testUserWithQuizItem(quiz)
  }

  // Don't save the small demo quiz. It should be completed in one sitting if at all.
  override def saveQuiz(quiz: Quiz) {}

}