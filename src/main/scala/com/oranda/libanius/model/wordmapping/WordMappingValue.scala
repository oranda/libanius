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

package com.oranda.libanius.model.wordmapping

import com.oranda.libanius.model.UserAnswer
import scala.xml.Unparsed
import scala.xml.Text
import com.oranda.libanius.Props
import android.util.Log
import com.oranda.libanius.model.QuizItemWithUserAnswers

case class WordMappingValue(val value: String) extends QuizItemWithUserAnswers { 
     
  override def toString = value   // e.g. "unterrichten"

  def toXML =
      <wordMappingValue value={value}>
        <userAnswers>{userAnswers map (u => u.toXML) }</userAnswers>
      </wordMappingValue>

  
  def hasSameStart(otherValue: String) = 
    (numOfLetters: Int) => otherValue != value && 
        value.take(numOfLetters) == otherValue.take(numOfLetters)
  
  def hasSameEnd(otherValue: String) = 
    (numOfLetters: Int) => otherValue != value && 
        value.takeRight(numOfLetters) == otherValue.takeRight(numOfLetters)
  
}

object WordMappingValue {
    def fromXML(node: xml.Node): WordMappingValue =
	  new WordMappingValue(value = (node \ "@value").text) {
        val answers = (node \ "userAnswers")
	    for (val userAnswer <- answers \\ "userAnswer")
	      addUserAnswer(Some(UserAnswer.fromXML(userAnswer)))
	  }
}