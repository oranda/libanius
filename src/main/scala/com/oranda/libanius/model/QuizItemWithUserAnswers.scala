/*
 * Copyright 2012 James McCabe <jamesc@oranda.com>
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
import com.oranda.libanius.Props

// TODO: this is an old class, to be updated
trait QuizItemWithUserAnswers {

  var userAnswers = List[UserAnswer]()
         
  def addUserAnswer(userAnswerOpt : Option[UserAnswer]) {
    userAnswerOpt match {
      case Some(userAnswer) => userAnswers :+= userAnswer
      case None =>
    }
  } 
      
  def isPresentable(numCorrectAnswersInARowDesired : Int, 
      diffInPromptNumMinimum : Int, currentPromptNum : Int) : Boolean = {
    
    if (numCorrectAnswersInARowDesired == -1    // special case: pick any
        && numCorrectAnswersInARow < Props.NUM_CORRECT_ANSWERS_REQUIRED)
      return true
    if (numCorrectAnswersInARow != numCorrectAnswersInARowDesired)
      return false
    if (userAnswers.isEmpty)
      return true
   
    val diffInPromptNum = currentPromptNum - promptNumInLastAnswer
    return diffInPromptNum >= diffInPromptNumMinimum
  }
  
  def numCorrectAnswersInARow: Int = {
    var count = 0
    val iter = userAnswers.reverse.elements
    var stop = false
    while (iter.hasNext && !stop) {
      val userAnswer = iter.next
      if (userAnswer.wasCorrect)
        count = count + 1
      else
        stop = true
    }
    return count
  }
    
  def promptNumInLastAnswer : Int = {
    val lastUserAnswer = userAnswers.last
    return lastUserAnswer.promptNumber
  }
}