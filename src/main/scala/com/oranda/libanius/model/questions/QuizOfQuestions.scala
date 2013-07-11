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

package com.oranda.libanius.model.questions

import scala.collection.mutable.LinkedHashSet
import com.oranda.libanius.model.Quiz

// This class is old, needs updating
class QuizOfQuestions(val currentPromptNumber: Int) extends Quiz(currentPromptNumber) {
  // TODO: convert to an immutable parameter
  private[this] val questionItems : LinkedHashSet[QuestionItem] = new LinkedHashSet()
  
  def numQuestionItems = questionItems.size
  
  def copy(newPromptNumber: Int) = new QuizOfQuestions(currentPromptNumber)
  
  def toXML =
    <quiz>
      <currentPromptNumber>{currentPromptNumber}</currentPromptNumber>
      <QuestionItems>{questionItems map (q => q.toXML) }</QuestionItems>
    </quiz>

  def toCustomFormat : String = 
    currentPromptNumber + "\n" +
        (questionItems map (q => q.toCustomFormat + "\n")).reduceLeft[String](_+_)
        
  // TODO: check duplicate questions are avoided
  def addItem(questionItemOpt : Option[QuestionItem]) {
    questionItems += questionItemOpt.getOrElse(questionItemOpt.get)
  }  
  
  def addItems(questionItemsOpt : Option[Tuple2[QuestionItem, QuestionItem]]) {
    questionItemsOpt.foreach { twoQuestionItems =>
        questionItems += twoQuestionItems._1  
        questionItems += twoQuestionItems._2
    }
  }
  
  def findQuestionItem(numCorrectAnswersInARowDesired : Int, diffInPromptNum : Int): 
      Option[QuestionItem] =
    questionItems.find(questionItem => questionItem.isPresentable(
        numCorrectAnswersInARowDesired, diffInPromptNum, currentPromptNumber))
    
  def remove(QuestionItem : QuestionItem) {
    questionItems -= QuestionItem
  }
  
  override def scoreSoFar : BigDecimal = {  // out of 1
    val availableScorePerItem = ( 1.0 / numQuestionItems): BigDecimal
    var score = 0: BigDecimal
    questionItems.foreach(questionItem => 
        score = score + (questionItem.scoreSoFar * availableScorePerItem))
    score
  }
  
  def numItems: Int = questionItems.size    
  
  def numCorrectAnswers: Int  = 0  // TODO
}

object QuizOfQuestions {
    
  def fromCustomFormat(strCustomFormat: String): QuizOfQuestions = {
    val lines = strCustomFormat.split("\\n")
    val currentPromptNumber = lines(0).toInt
    new QuizOfQuestions(currentPromptNumber) {  
      lines.foreach(line => 
        if (line.contains("§§")) 
          addItem(Some(QuestionItem.fromCustomFormat(line))))
    }
  }

}
