/*
 * Copyright 2012 James McCabe <james@oranda.com>
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
import android.util.Log
import com.oranda.libanius.util.StringSplitter

trait QuizItemWithUserAnswers extends ModelComponent {

  protected var correctAnswersInARow = List[UserAnswer]()
  protected var incorrectAnswers = List[UserAnswer]()
  
  def userAnswers = correctAnswersInARow ++ incorrectAnswers
  
  def addUserAnswer(userAnswer : UserAnswer) {
    if (userAnswer.wasCorrect) {
      correctAnswersInARow ::= userAnswer 
    } else {
      correctAnswersInARow = List() // old correct answers are discarded
      incorrectAnswers ::= userAnswer
    }
  }
  
  def addUserAnswersBatch(correctPromptNumStrs: List[String], 
      incorrectPromptNumStrs: List[String]) {
    // TODO: see if an imperative version is faster
    correctAnswersInARow = correctPromptNumStrs.map(correctPromptNum =>
        new UserAnswer(wasCorrect = false, promptNumber = correctPromptNum.toInt))
    incorrectAnswers = incorrectPromptNumStrs.map(incorrectPromptNum =>
        new UserAnswer(wasCorrect = false, promptNumber = incorrectPromptNum.toInt))
  }
  
  def isPresentable(currentPromptNum : Int): Boolean = {
    /*
     * See if this quiz item meets defined criteria: how many times 
     * it has been answered correctly, and how long ago it was last answered.
     * Try different pairs of values for these criteria until a quiz item fits.
     */
    val criteriaSets = Seq((1, 5), (2, 40), (3, 800), /*(4, 5000),*/ (0, -1))
    criteriaSets.exists(criteria => 
      isPresentable(currentPromptNum, criteria._1, criteria._2))
  }
  
  def isPresentable(currentPromptNum : Int, 
      numCorrectAnswersInARowDesired: Int, diffInPromptNumMinimum: Int): Boolean = {
    def diffInPromptNum = currentPromptNum - promptNumInLastAnswer
    numCorrectAnswersInARow == numCorrectAnswersInARowDesired &&
      (correctAnswersInARow.isEmpty && incorrectAnswers.isEmpty ||
      diffInPromptNum >= diffInPromptNumMinimum)
  }
  
  def isUnfinished: Boolean = 
    numCorrectAnswersInARow < Props.NUM_CORRECT_ANSWERS_REQUIRED
  
  def numCorrectAnswersInARow = correctAnswersInARow.length
  
  def promptNumInLastAnswer: Int = {
    if (!correctAnswersInARow.isEmpty)
      correctAnswersInARow.last.promptNumber
    else if (!incorrectAnswers.isEmpty)
      incorrectAnswers.last.promptNumber
    else
      Int.MinValue
  }
}