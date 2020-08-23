/*
 * Libanius
 * Copyright (C) 2012-2020 James McCabe <jjtmccabe@gmail.com>
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

package com.oranda.libanius.consoleui

import Output._
import com.oranda.libanius.model._

object DemoQuiz extends InteractiveQuiz {

  runQuiz()

  def runQuiz(): Unit = {

    output("Running demo quiz...")
    val quiz = Quiz.demoQuiz()
    output("OK, the quiz begins! To quit, type q at any time.\n")
    testUserWithQuizItem(quiz)
  }

  // Don't save the small demo quiz. It should be completed in one sitting if at all.
  override def saveQuiz(quiz: Quiz): Unit = {}
}
