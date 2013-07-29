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

package com.oranda.libanius.model.wordmapping

import scala.util.Random

/**
 * Quiz item data holder:
 * contains whatever information is necessary for the view, and for updating the backing data.
 */
case class QuizItemViewWithOptions(val keyWord: String,
    val wmvs: WordMappingValueSet,
    val wordMappingValue: WordMappingValue,
    val wmgCurrentPromptNumber: Int,
    val quizGroupHeader: QuizGroupHeader,
    val falseAnswers: Set[String],
    val numCorrectAnswersInARow: Int) {

  lazy val keyType = quizGroupHeader.keyType
  lazy val valueType = quizGroupHeader.valueType

  lazy val allOptions = optionsInRandomOrder(wordMappingValue, falseAnswers)

  def optionsInRandomOrder(wmv: WordMappingValue, otherOptions: Set[String]): List[String] = {
    val allOptions = otherOptions + wmv.value
    Random.shuffle(allOptions.toList)
  }
}
