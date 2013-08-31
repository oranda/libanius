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

package com.oranda.libanius.model

import scala.util.Random

/**
 * Quiz item data holder:
 * contains whatever information is necessary for the view, and for updating the backing data.
 */
class QuizItemViewWithChoices(
    val quizPair: QuizPair,
    val quizValue: QuizValueWithUserAnswers,
    val qgCurrentPromptNumber: Int,
    val quizGroupHeader: QuizGroupHeader,
    val falseAnswers: Set[String],
    val numCorrectAnswersInARow: Int) {

  lazy val cue = quizPair.cue
  lazy val response = quizPair.response
  lazy val cueType = quizGroupHeader.cueType
  lazy val responseType = quizGroupHeader.responseType

  lazy val allChoices = choicesInRandomOrder(quizValue, falseAnswers)

  def choicesInRandomOrder(quizValue: QuizValueWithUserAnswers, otherChoices: Set[String]):
      List[String] = {
    val allChoices = otherChoices + quizValue.value
    Random.shuffle(allChoices.toList)
  }

  // In the future, quiz items with many correct answers might not be multiple choice.
  def useMultipleChoice = true
}
