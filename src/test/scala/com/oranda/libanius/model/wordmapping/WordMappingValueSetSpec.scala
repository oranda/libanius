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

import org.specs2.mutable.Specification
import com.oranda.libanius.Props
import org.specs2.specification.Scope

class WordMappingValueSetSpec extends Specification {

  "a word-mapping-value-set" should {
    
    val wmvsCustomFormat = "contract:696,698;697/treaty:796;798"
    
      /*
    val wmvsXml = <wordMapping key="Vertrag">
      <wordMappingValue value="contract">
        <userAnswers>
          <userAnswer wasCorrect="true" promptNumber="696"/>
          <userAnswer wasCorrect="false" promptNumber="697"/>
          <userAnswer wasCorrect="true" promptNumber="698"/>
        </userAnswers>
      </wordMappingValue>
      <wordMappingValue value="treaty">
        <userAnswers>
          <userAnswer wasCorrect="true" promptNumber="796"/>
          <userAnswer wasCorrect="false" promptNumber="798"/>
        </userAnswers>
      </wordMappingValue>
    </wordMapping>
    */    
    
    Props.ANDROID = false
    
    val wmvs = WordMappingValueSet.fromCustomFormat(wmvsCustomFormat) 
    //val wmvsFromXml = WordMappingValueSet.fromXML(wmvsXml)
    
    sequential 
    
    "be parseable from custom format" in {
      wmvs.containsValue("treaty")
      wmvs.size mustEqual 2
      wmvs.toCustomFormat(new StringBuilder("")).toString mustEqual wmvsCustomFormat
    }
    
    /*
    "be parseable from XML" in {
      wmvsFromXml.containsValue("treaty")
      wmvsFromXml.size mustEqual 2  
    }*/
      
    "allow a word-mapping-value to be added" in {
      val wmvsLocal = WordMappingValueSet.fromCustomFormat(wmvsCustomFormat)
      val wmvsNew = wmvsLocal.addValueToEnd(new WordMappingValue("agreement"))
      wmvsNew.size mustEqual 3
    }
    
    "identify a word-mapping-value that is presentable in the context of the quiz" in {
      val wmvsLocal = WordMappingValueSet.fromCustomFormat(wmvsCustomFormat)
      val correctAnswers = wmvsLocal.numCorrectAnswers
      val wmvOpt = wmvsLocal.findPresentableWordMappingValue(currentPromptNumber = 800)
      wmvOpt.isDefined mustEqual true
      wmvOpt.get.value mustEqual "contract"
    }      
  }
  
}  