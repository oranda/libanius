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
import com.oranda.libanius.dependencies.Conf
import com.oranda.libanius.model.Quiz

object ReadVocabFileTwoListsTest {

  def main(args: Array[String]) {
   /* 
    val quizXml = 
<quiz currentPromptNumber="0">
  <quizGroup type="WordMapping" keyType="EnglishWord" valueType="GermanWord">
    <wordMapping key="against">  
      <quizValue value="wider"><userAnswers></userAnswers></quizValue>
    </wordMapping>
    <wordMapping key="entertain">  
      <quizValue value="unterhalten"><userAnswers></userAnswers></quizValue>
    </wordMapping>
    <wordMapping key="teach">
      <quizValue value="unterrichten"><userAnswers></userAnswers></quizValue>
    </wordMapping>
    <wordMapping key="winner"> 
      <quizValue value="Siegerin"><userAnswers></userAnswers></quizValue>
    </wordMapping>
    <wordMapping key="en route"> 
      <quizValue value="unterwegs"><userAnswers></userAnswers></quizValue>
    </wordMapping>
    <wordMapping key="full">
      <quizValue value="satt"><userAnswers></userAnswers></quizValue>
      <quizValue value="voll"><userAnswers></userAnswers></quizValue>
    </wordMapping>
    <wordMapping key="interrupted"> 
      <quizValue value="unterbrochen"><userAnswers></userAnswers></quizValue>
    </wordMapping>
    <wordMapping key="contract">
      <quizValue value="Vertrag"><userAnswers></userAnswers></quizValue>
    </wordMapping>
    <wordMapping key="rides"> 
      <quizValue value="reitet"><userAnswers></userAnswers></quizValue>
    </wordMapping>
    <wordMapping key="sweeps">  
      <quizValue value="streicht"><userAnswers></userAnswers></quizValue>
    </wordMapping>
  </wordMappingGroup>
  <quizGroup type="WordMapping" keyType="GermanWord" valueType="EnglishWord">
    <wordMapping key="unterwegs">
      <quizValue value="en route"><userAnswers></userAnswers></quizValue>
    </wordMapping>
    <wordMapping key="Vertrag">
      <quizValue value="contract"><userAnswers></userAnswers></quizValue>
      <quizValue value="treaty"><userAnswers></userAnswers></quizValue>
    </wordMapping>
  </wordMappingGroup>
</quiz>

    Conf.conf.ANDROID = false
   
    val quiz = QuizOfWordMappings.fromXML(quizXml)
    
    val quizXmlNew: xml.Node = quiz.toXML
    
    print(new PrettyPrinter(999, 2).format(quizXmlNew)) // just an eyeball test for now
    
    */
  }
}