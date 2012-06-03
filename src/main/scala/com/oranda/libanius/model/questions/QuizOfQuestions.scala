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

package com.oranda.libanius.model.questions

import _root_.scala.collection.mutable.ListBuffer
import _root_.java.io.File
import _root_.scala.collection.mutable.LinkedHashSet
import _root_.java.io.FileOutputStream
import com.oranda.libanius.Props
import com.oranda.libanius.model.Quiz

// This class is old, needs updating
class QuizOfQuestions(_currentPromptNumber: Int) extends Quiz(_currentPromptNumber) {
  private[this] val questionItems : LinkedHashSet[QuestionItem] = new LinkedHashSet()
  
  def numQuestionItems = questionItems.size
  
  def toXML =
    <quiz>
      <currentPromptNumber>{_currentPromptNumber}</currentPromptNumber>
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
    questionItemsOpt match {
      case Some((item1, item2)) => 
        questionItems += item1  
        questionItems += item2
      case None => 
    }
  }
  
  def findQuestionItem(numCorrectAnswersInARowDesired : Int, diffInPromptNum : Int): 
      Option[QuestionItem] = {
    return questionItems.find(questionItem => questionItem.isPresentable(
        numCorrectAnswersInARowDesired, diffInPromptNum, currentPromptNumber))
  }
    
  def delete(QuestionItem : QuestionItem) {
    questionItems -= QuestionItem
  }
  
  override def scoreSoFar : BigDecimal = {  // out of 1
    val availableScorePerItem = ( 1.0 / numQuestionItems) : BigDecimal
    var score = 0 : BigDecimal
    questionItems.foreach(questionItem => 
        score = score + (questionItem.scoreSoFar * availableScorePerItem))
    return score
  }
  
  def numItems: Int = questionItems.size    
  
  def numCorrectAnswers: Int  = 0  // TODO
}

object QuizOfQuestions {
  def fromXML(node: xml.Node): QuizOfQuestions =
	new QuizOfQuestions(
	    _currentPromptNumber = (node \ "currentPromptNumber").text.toInt) {	    
	  val quizItemsXml = (node \ "quizItems")
	  for (quizItemXml <- quizItemsXml \\ "quizItem")
	    addItem(Some(QuestionItem.fromXML(quizItemXml)))
	}
    
  def fromCustomFormat(strCustomFormat: String): QuizOfQuestions = 
    new QuizOfQuestions(_currentPromptNumber = 0) {
      val lines = strCustomFormat.split("\\n")
      currentPromptNumber = lines(0).toInt
      lines.foreach(line => if (line.contains("§§")) addItem(
          Some(QuestionItem.fromCustomFormat(line))))
    }
}
