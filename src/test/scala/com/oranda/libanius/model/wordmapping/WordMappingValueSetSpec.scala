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

class WordMappingValueSetSpec extends Specification {

  "a word-mapping-value-set" should {
    
    val wmvXml = <wordMapping key="Vertrag">
      <wordMappingValue value="contract"><userAnswers><userAnswer wasCorrect="TRUE" promptNumber="1"/></userAnswers></wordMappingValue>
      <wordMappingValue value="treaty"><userAnswers></userAnswers></wordMappingValue>
    </wordMapping>
  
    val wmvs = WordMappingValueSet.fromXML(wmvXml)
    
    "be parseable from XML" in {
      wmvs.containsValue("treaty")
      wmvs.size mustEqual 2 // ?? 
    }
      
    "allow a word-mapping-value to be added" in {
      val wmvsLocal = WordMappingValueSet.fromXML(wmvXml)
      wmvsLocal.addWordMappingValue(Some(new WordMappingValue("agreement")))
      wmvsLocal.size mustEqual 3
    }    
   
    "identify a word-mapping-value that is presentable in the context of the quiz" in {
      val wmv = wmvs.findPresentableWordMappingValue(numCorrectAnswersInARowDesired = 1,
          diffInPromptNumMinimum = 2, currentPromptNumber = 3)
      wmv.get.value mustEqual "contract"
    }
      
  }
  
  
}