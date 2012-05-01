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

class QuizOfWordMappingsSpec extends Specification {

  "a quiz of word-mappings" should {
    
    val quizXml = <quizOfWordMappings currentPromptNumber="0">
  <wordMappingGroup keyType="English word" valueType="German word">
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
  <wordMappingGroup keyType="German word" valueType="English word">
    <wordMapping key="unterwegs">
      <wordMappingValue value="en route"><userAnswers></userAnswers></wordMappingValue>
    </wordMapping>
    <wordMapping key="Vertrag">
      <wordMappingValue value="contract"><userAnswers></userAnswers></wordMappingValue>
      <wordMappingValue value="treaty"><userAnswers></userAnswers></wordMappingValue>
    </wordMapping>
  </wordMappingGroup>
</quizOfWordMappings>
  
    
    val quiz = QuizOfWordMappings.fromXML(quizXml)
     
    "be parseable from XML" in {
      quiz.currentPromptNumber mustEqual 0
      val wmg = quiz.findWordMappingGroup(keyType = "German word", 
          valueType = "English word")
      wmg.isDefined mustEqual true
    }
    
    "find a presentable quiz item" in {
      val quizItem = quiz.findQuizItem(numCorrectAnswersInARowDesired = 0, 
          diffInPromptNum = 0)
      quizItem.isDefined mustEqual true
    }
    
    "offer translations for a word, given the group of the word" in { 
      val translations = quiz.findValuesFor(keyWord = "Vertrag", 
          keyType = "German word", valueType = "English word")
      translations.contains("contract") mustEqual true
      translations.contains("treaty") mustEqual true
    }    
    
    "allow key-words to be deleted from a particular group" in {
      val quizLocal = QuizOfWordMappings.fromXML(quizXml)
      quizLocal.deleteWord("full", "English word", "German word")
      val wmg = quizLocal.findWordMappingGroup(keyType = "English word", 
          valueType = "German word").get
      wmg.contains("full") mustEqual false
    }
    
    "contain unique groups only" in {
      val quizLocal = QuizOfWordMappings.fromXML(quizXml)
      quizLocal.numGroups == 2 // precondition
      val wmg = WordMappingGroup("English word", "German word")
      quizLocal.addWordMappingGroup(wmg) // should have no effect
      quizLocal.numGroups == 2
    }
  }
}