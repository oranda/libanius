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

/*
 * A connection between two things, and user information associated with the connection.
 *
 * Examples of quiz pairs:
 *  - a question and an answer in the quiz
 *  - a word and a translation
 *
 * TODO: separate QuizValueWithUserAnswers into value and UserAnswers
 */
case class QuizPair(key: String, value: QuizValueWithUserAnswers) {
  def sameKeyValue(other: QuizPair) = other.key == key && other.value.value == value.value
}

object QuizPair {
  def apply(key: String, value: String): QuizPair =
    QuizPair(key, QuizValueWithUserAnswers(value))
}