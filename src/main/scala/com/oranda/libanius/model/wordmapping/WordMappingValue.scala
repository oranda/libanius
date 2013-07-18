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

import com.oranda.libanius.model.QuizItemWithUserAnswers
import com.oranda.libanius.model.UserAnswer
import com.oranda.libanius.util.StringUtil
import com.oranda.libanius.util.Platform
import com.oranda.libanius.util.StringSplitter

case class WordMappingValue(val value: String, val correctAnswersInARow: List[UserAnswer] = Nil,
                            val incorrectAnswers: List[UserAnswer] = Nil)
    extends QuizItemWithUserAnswers[WordMappingValue](correctAnswersInARow, incorrectAnswers) {

  def self = this

  def updated(correctAnswersInARow: List[UserAnswer], incorrectAnswers: List[UserAnswer]):
      WordMappingValue =
    WordMappingValue(value, correctAnswersInARow, incorrectAnswers)

  override def toString = value   // e.g. "unterrichten"
  
  // Example: nachlösen:1,7,9;6
  def toCustomFormat(strBuilder: StringBuilder): StringBuilder = {
    strBuilder.append(value)
    
    if (!correctAnswersInARow.isEmpty || !incorrectAnswers.isEmpty)
      strBuilder.append(':')
    if (!correctAnswersInARow.isEmpty)
      StringUtil.mkString(strBuilder, correctAnswersInARow, answerPromptNumber, ',')
    if (!incorrectAnswers.isEmpty) {
      strBuilder.append(';')
      StringUtil.mkString(strBuilder, incorrectAnswers, answerPromptNumber, ',')
    }
    strBuilder
  }
  
  def answerPromptNumber(strBuilder: StringBuilder, answer: UserAnswer) = 
    strBuilder.append(answer.promptNumber)

  
  def hasSameStart(otherValue: String) = 
    (numOfLetters: Int) => otherValue != value && 
        value.take(numOfLetters) == otherValue.take(numOfLetters)
  
  def hasSameEnd(otherValue: String) = 
    (numOfLetters: Int) => otherValue != value && 
        value.takeRight(numOfLetters) == otherValue.takeRight(numOfLetters)

}

object WordMappingValue extends Platform {
  
  /**
   * The String processing here needs to be very fast. 
   * The Android splitter utilities are faster than String.split()
   */
  val wmvSplitter = getSplitter(':')
  val allAnswersSplitter = getSplitter(';')
  val answersSplitter = getSplitter(',')
  
  // Example: str = "nachlösen:1,7,9;6"
  def fromCustomFormat(str: String): WordMappingValue = {
    wmvSplitter.setString(str)
    var wmv = new WordMappingValue(wmvSplitter.next)
    if (wmvSplitter.hasNext) {
      val strAllAnswers = wmvSplitter.next
      allAnswersSplitter.setString(strAllAnswers)

      val correctPromptNums = allAnswersSplitter.next
      answersSplitter.setString(correctPromptNums)
      val correctAnswers = answersSplitter.toList
      val incorrectAnswers =
          if (allAnswersSplitter.hasNext) {
            val incorrectPromptNums = allAnswersSplitter.next
            answersSplitter.setString(incorrectPromptNums)
            answersSplitter.toList
          } else
            List()
      wmv = wmv.addUserAnswersBatch(correctAnswers, incorrectAnswers)
    }
    wmv
  }
}