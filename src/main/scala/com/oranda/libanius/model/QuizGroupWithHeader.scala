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

import scala.language.implicitConversions

/*
 * Convenience class for passing around a key-value pair from the Quiz.quizGroups map.
 */
case class QuizGroupWithHeader(header: QuizGroupHeader, quizGroup: QuizGroup) {
  def toPair = Pair(header, quizGroup)
}

object QuizGroupWithHeader {
  // Forward access to QuizGroupHeader whenever necessary
  implicit def qgwh2qgh(qgwh: QuizGroupWithHeader): QuizGroupHeader = qgwh.header

  // Forward access to QuizGroup whenever necessary
  implicit def qgwh2qg(qgwh: QuizGroupWithHeader): QuizGroup = qgwh.quizGroup
}

