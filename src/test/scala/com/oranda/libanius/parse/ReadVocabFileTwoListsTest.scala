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

package com.oranda.libanius.parse

import scala.xml.PrettyPrinter
import com.oranda.libanius.model.wordmapping.QuizOfWordMappings
import com.oranda.libanius.dependencies.Conf

object ReadVocabFileTwoListsTest {

  def main(args: Array[String]) {
   /* 
    val quizXml = 
<quizOfWordMappings currentPromptNumber="0">
  <wordMappingGroup keyType="EnglishWord" valueType="GermanWord">
    <wordMapping key="against">  
      <wordMappingValue value="wider"><userAnswers></userAnswers></wordMappingValue> 
    </wordMapping>
    <wordMapping key="entertain">  
      <wordMappingValue value="unterhalten"><userAnswers></userAnswers></wordMappingValue> 
    </wordMapping>
    <wordMapping key="teach">
      <wordMappingValue value="unterrichten"><userAnswers></userAnswers></wordMappingValue> 
    </wordMapping>
    <wordMapping key="winner"> 
      <wordMappingValue value="Siegerin"><userAnswers></userAnswers></wordMappingValue> 
    </wordMapping>
    <wordMapping key="en route"> 
      <wordMappingValue value="unterwegs"><userAnswers></userAnswers></wordMappingValue> 
    </wordMapping>
    <wordMapping key="full">
      <wordMappingValue value="satt"><userAnswers></userAnswers></wordMappingValue> 
      <wordMappingValue value="voll"><userAnswers></userAnswers></wordMappingValue>   
    </wordMapping>
    <wordMapping key="interrupted"> 
      <wordMappingValue value="unterbrochen"><userAnswers></userAnswers></wordMappingValue> 
    </wordMapping>
    <wordMapping key="contract">
      <wordMappingValue value="Vertrag"><userAnswers></userAnswers></wordMappingValue>        
    </wordMapping>
    <wordMapping key="rides"> 
      <wordMappingValue value="reitet"><userAnswers></userAnswers></wordMappingValue> 
    </wordMapping>
    <wordMapping key="sweeps">  
      <wordMappingValue value="streicht"><userAnswers></userAnswers></wordMappingValue> 
    </wordMapping>
  </wordMappingGroup>
  <wordMappingGroup keyType="GermanWord" valueType="EnglishWord">
    <wordMapping key="unterwegs">
      <wordMappingValue value="en route"><userAnswers></userAnswers></wordMappingValue>
    </wordMapping>
    <wordMapping key="Vertrag">
      <wordMappingValue value="contract"><userAnswers></userAnswers></wordMappingValue>
      <wordMappingValue value="treaty"><userAnswers></userAnswers></wordMappingValue>
    </wordMapping>
  </wordMappingGroup>
</quizOfWordMappings>

    Conf.conf.ANDROID = false
   
    val quiz = QuizOfWordMappings.fromXML(quizXml)
    
    val quizXmlNew: xml.Node = quiz.toXML
    
    print(new PrettyPrinter(999, 2).format(quizXmlNew)) // just an eyeball test for now
    
    */
  }
}