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
  <quizGroup type="WordMapping" cueType="EnglishWord" responseType="GermanWord">
    <wordMapping cue="against">
      <quizValue response="wider"><userAnswers></userAnswers></quizValue>
    </wordMapping>
    <wordMapping cue="entertain">
      <quizValue response="unterhalten"><userAnswers></userAnswers></quizValue>
    </wordMapping>
    <wordMapping cue="teach">
      <quizValue response="unterrichten"><userAnswers></userAnswers></quizValue>
    </wordMapping>
    <wordMapping cue="winner">
      <quizValue response="Siegerin"><userAnswers></userAnswers></quizValue>
    </wordMapping>
    <wordMapping cue="en route">
      <quizValue response="unterwegs"><userAnswers></userAnswers></quizValue>
    </wordMapping>
    <wordMapping cue="full">
      <quizValue response="satt"><userAnswers></userAnswers></quizValue>
      <quizValue response="voll"><userAnswers></userAnswers></quizValue>
    </wordMapping>
    <wordMapping cue="interrupted">
      <quizValue response="unterbrochen"><userAnswers></userAnswers></quizValue>
    </wordMapping>
    <wordMapping cue="contract">
      <quizValue response="Vertrag"><userAnswers></userAnswers></quizValue>
    </wordMapping>
    <wordMapping cue="rides">
      <quizValue response="reitet"><userAnswers></userAnswers></quizValue>
    </wordMapping>
    <wordMapping cue="sweeps">
      <quizValue response="streicht"><userAnswers></userAnswers></quizValue>
    </wordMapping>
  </wordMappingGroup>
  <quizGroup type="WordMapping" cueType="GermanWord" responseType="EnglishWord">
    <wordMapping cue="unterwegs">
      <quizValue response="en route"><userAnswers></userAnswers></quizValue>
    </wordMapping>
    <wordMapping cue="Vertrag">
      <quizValue response="contract"><userAnswers></userAnswers></quizValue>
      <quizValue response="treaty"><userAnswers></userAnswers></quizValue>
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