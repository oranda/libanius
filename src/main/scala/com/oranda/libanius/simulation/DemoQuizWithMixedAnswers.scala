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

package com.oranda.libanius.simulation

import com.oranda.libanius.model.quizitem.QuizItemViewWithChoices
import com.oranda.libanius.model.Quiz

object DemoQuizWithMixedAnswers extends App with DemoQuiz {

  runQuiz()

  /**
   * Give wrong and right answers according to a schedule, changing the pattern every
   * hundred prompts, and keep cycling.
   */
  override def makeResponse(quizItem: QuizItemViewWithChoices, quiz: Quiz,
      responsesProcessed: Long): String =
    (responsesProcessed / 100) % 3 match {
      // intervals should stay about the same
      case 0 => if (responsesProcessed % 10 < 8) quizItem.correctResponse.value else "wrong"
      // intervals should narrow
      case 1 => if (responsesProcessed % 10 < 5) quizItem.correctResponse.value else "wrong2"
      // intervals should widen
      case 2 => quizItem.correctResponse.value
    }

}
