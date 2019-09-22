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

package com.oranda.libanius.model.quizitem

import org.specs2.mutable.Specification
import com.oranda.libanius.dependencies.AppDependencyAccess
import com.oranda.libanius.model.TestData._

class QuizItemSpec extends Specification with AppDependencyAccess {

  "a quiz item" should {

    "be presentable in the quiz, given certain criteria" in {
      val repetitionInterval = 2
      quizItem.isPresentable(currentPromptNumber = 11, repetitionInterval) mustEqual true
      quizItem.isPresentable(currentPromptNumber = 10, repetitionInterval) mustEqual false
    }

    "be matchable against another by the first few letters" in {
      quizItem.correctResponse.hasSameStart("nachfahren")(4) mustEqual true
      quizItem.correctResponse.hasSameStart("nachfahren")(5) mustEqual false
    }

    "be matchable against another by the last few letters" in {
      quizItem.correctResponse.hasSameEnd("nachfahren")(2) mustEqual true
      quizItem.correctResponse.hasSameEnd("nachfahren")(3) mustEqual false
    }

    "know the number of times in a row it was answered correctly by the user" in {
      quizItem.numCorrectResponsesInARow must be equalTo 2
    }

    "be matchable even if there is a 'to ' at the front" in {
      val quizItem = QuizItem("leiden", "to suffer")
      quizItem.looselyMatches("suffer") mustEqual true
    }
  }
}
