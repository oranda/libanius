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


case class QuestionItem() {
  /*
 TODO
  
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

  */
}
