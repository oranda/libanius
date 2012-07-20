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

import scala.xml.Node
import com.oranda.libanius.Props
import com.oranda.libanius.model.UserAnswer
import scala.util.matching.Regex
import com.oranda.libanius.model.QuizItemWithUserAnswers

class QuestionItem extends QuizItemWithUserAnswers {
  var question: String = _
  var correctAnswer: String = _
  
  override def equals(other: Any) = other match {
    case that: QuestionItem =>
      this.question == that.question
      case _ => false
  }
  override def hashCode = question.hashCode
  
  def addUserAnswer(userAnswer : Node) {
    val newAnswer = UserAnswer.fromXML(userAnswer)
    addUserAnswer(newAnswer)
  }
  
  def evaluateUserAnswer(promptNum: Int, userAnswerStr: String): Boolean = {

    val wasCorrectAnswer = isCorrect(userAnswerStr)

    val userAnswer = new UserAnswer(wasCorrect = wasCorrectAnswer,
        promptNumber = promptNum)
    addUserAnswer(userAnswer)
    
    wasCorrectAnswer
  }
  
  def isCorrect(userAnswerStr: String): Boolean =    
    userAnswerStr == correctAnswer || isSimilarToCorrectAnswer(userAnswerStr)
    
  def isSimilarToCorrectAnswer(userAnswerStr: String): Boolean = {
    var correctAnswerMod = correctAnswer
    
    if (correctAnswerMod.contains("/"))
      correctAnswerMod = correctAnswerMod.substring(0, correctAnswerMod.indexOf("/"))
    
    var userAnswerStrMod = userAnswerStr.replaceAll(",", "")
    correctAnswerMod = correctAnswerMod.replaceAll(",", "")
    userAnswerStrMod = userAnswerStr.replaceAll("\\.", "")
    correctAnswerMod = correctAnswerMod.replaceAll("\\.", "") 
    userAnswerStrMod = userAnswerStr.replaceAll("!", "")
    correctAnswerMod = correctAnswerMod.replaceAll("!", "")    
    userAnswerStrMod = userAnswerStrMod.trim
    correctAnswerMod = correctAnswerMod.trim
    
    correctAnswerMod = correctAnswerMod.toLowerCase()
    userAnswerStrMod = userAnswerStrMod.toLowerCase()  
      
    // Capture all Strings outside brackets. It will be the 1st group in each match.
    // Test here:  http://blog.logiclabz.com/tools/online-regex-checker.aspx
    val pattern = new Regex("""([\s|\w]+)(\(|\z)""")
    val matchIter = pattern.findAllIn(correctAnswerMod)
    val correctAnswerRequiredParts 
    		= matchIter.matchData.toList map { m => m.subgroups(0).trim()}
    allPartsHaveMatchIn(correctAnswerRequiredParts, userAnswerStrMod)
  }
  
  protected def allPartsHaveMatchIn(parts: List[String], str: String): Boolean = {
    var strVar = str
    parts.foreach { part =>
      strVar match {
        case s if s.contains(part) => 
          val indexOfPartEnd = s.indexOf(part) + part.length
          strVar = s.substring(indexOfPartEnd)
        case _ => return false 
      }        
    }
    true
  }
  
  def scoreSoFar : BigDecimal =  // out of 1
    numCorrectAnswersInARow.toFloat / Props.NUM_CORRECT_ANSWERS_REQUIRED : BigDecimal
    
  def toXML =
<quizItem>
  <question>{question}</question>
  <answer>{correctAnswer}</answer>
  <userAnswers>{userAnswers map (u => u.toXML) }</userAnswers>
</quizItem>
 
  def toCustomFormat : String = {
    var customFormat = question + "§§" + correctAnswer + "§§"
    userAnswers.toSeq match {
      case Nil => 
      case _ => customFormat += 
        (userAnswers map (u => u.toCustomFormat + ":")).reduceLeft[String](_+_)
    }
    customFormat
  }  
}


object QuestionItem {
    def fromXML(node: xml.Node): QuestionItem =
	  new QuestionItem {
	    question = (node \ "question").text
	    correctAnswer = (node \ "answer").text
	    val answers = (node \ "userAnswers")
	    for (userAnswer <- answers \\ "userAnswer")
	      addUserAnswer(userAnswer)
	  }
    
    def fromCustomFormat(strCustomFormat: String): QuestionItem = 
      new QuestionItem {
        val parts = strCustomFormat.split("§§")
        question = parts(0)
        correctAnswer = parts(1)
        if (parts.length > 2) {
          val answersText = parts(2)
          val answersStrings = answersText.split(":")
          answersStrings.foreach(answerString => 
            addUserAnswer(UserAnswer.fromCustomFormat(answerString)))
        }
    }
}