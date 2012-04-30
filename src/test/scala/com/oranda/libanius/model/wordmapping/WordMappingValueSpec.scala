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

import org.specs2.mutable.Specification

class WordMappingValueSpec extends Specification {

  "a word-mapping-value" should {
    
    val wmvXml = 
      <wordMappingValue value="nachlösen">
        <userAnswers>
          <userAnswer wasCorrect="true" promptNumber="1"></userAnswer>
          <userAnswer wasCorrect="false" promptNumber="6"></userAnswer>
          <userAnswer wasCorrect="true" promptNumber="7"></userAnswer>
          <userAnswer wasCorrect="true" promptNumber="9"></userAnswer>
        </userAnswers>
      </wordMappingValue>
  
    val wmv = WordMappingValue.fromXML(wmvXml)
     
    "be parseable from XML" in {
      wmv.value mustEqual "nachlösen"
      wmv.userAnswers.length mustEqual 4
    }
    
    "be matchable against another by the first few letters" in {
      wmv.hasSameStart("nachfahren")(4) mustEqual true
      wmv.hasSameStart("nachfahren")(5) mustEqual false
    }
    
    "be matchable against another by the last few letters" in {
      wmv.hasSameEnd("nachfahren")(2) mustEqual true
      wmv.hasSameEnd("nachfahren")(3) mustEqual false
    }
    
    "know the number of times in a row it was answered correctly by the user" in {
      wmv.numCorrectAnswersInARow must be equalTo 2
    }
    
    "is presentable in the quiz, given certain criteria" in {
      wmv.isPresentable(numCorrectAnswersInARowDesired = 2,
          diffInPromptNumMinimum = 2, currentPromptNum = 11) mustEqual true
      wmv.isPresentable(numCorrectAnswersInARowDesired = 3,
          diffInPromptNumMinimum = 2, currentPromptNum = 11) mustEqual false
      wmv.isPresentable(numCorrectAnswersInARowDesired = 2,
          diffInPromptNumMinimum = 3, currentPromptNum = 11) mustEqual false
      wmv.isPresentable(numCorrectAnswersInARowDesired = 2,
          diffInPromptNumMinimum = 2, currentPromptNum = 10) mustEqual false
    }
  }
  
}