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
import com.oranda.libanius.io.StandardIO
import com.oranda.libanius.Props

class QuizOfWordMappingsSpec extends Specification {
  
  "a quiz of word-mappings" should {
    
    val quizCustomFormat = "quizOfWordMappings currentPromptNumber=\"0\"\n" +
        "wordMappingGroup keyType=\"English word\" valueType=\"German word\"\n" +
        "against|wider\n" +
        "entertain|unterhalten\n" +
        "teach|unterrichten\n" +
        "winner|Siegerin\n" +
        "en route|unterwegs\n" +
        "full|satt/voll\n" +
        "interrupted|unterbrochen\n" +
        "contract|Vertrag\n" +
        "rides|reitet\n" +
        "sweeps|streicht\n" +
        "wordMappingGroup keyType=\"German word\" valueType=\"English word\"\n" +
        "unterwegs|en route\n" +
        "Vertrag|contract:696,697;698/treaty:796;798" 
/*    
    val quizXml = <quizOfWordMappings currentPromptNumber="0">
  <wordMappingGroup keyType="English word" valueType="German word">
    <wordMapping key="against">  
      <wordMappingValue value="wider">
        <userAnswers>
          <userAnswer wasCorrect="true" promptNumber="696"/>
          <userAnswer wasCorrect="false" promptNumber="697"/>
        </userAnswers>
      </wordMappingValue> 
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
      <wordMappingValue value="contract">
        <userAnswers>
          <userAnswer wasCorrect="true" promptNumber="696"/>
          <userAnswer wasCorrect="true" promptNumber="697"/>
          <userAnswer wasCorrect="false" promptNumber="698"/>
        </userAnswers>
      </wordMappingValue>
      <wordMappingValue value="treaty">
        <userAnswers>
          <userAnswer wasCorrect="true" promptNumber="796"/>
          <userAnswer wasCorrect="false" promptNumber="798"/>
        </userAnswers>
      </wordMappingValue>
    </wordMapping>
  </wordMappingGroup>
</quizOfWordMappings>
  */
    Props.ANDROID = false
    
    val quiz = QuizOfWordMappings.fromCustomFormat(quizCustomFormat)
    //val quizFromXml = QuizOfWordMappings.fromXML(quizXml)
    
    sequential 
    
    "be parseable from custom format" in {
      quiz.currentPromptNumber mustEqual 0
      val wmg = quiz.findWordMappingGroup(keyType = "German word", valueType = "English word")
      wmg.isDefined mustEqual true
      quiz.toCustomFormat.toString mustEqual quizCustomFormat
    }
    
    /*
     * deprecated
     
    "be parseable from XML" in {
      quizFromXml.currentPromptNumber mustEqual 0
      val wmg = quiz.findWordMappingGroup(keyType = "German word", 
          valueType = "English word")
      wmg.isDefined mustEqual true
    }*/
    
    /* TODO
    "find a presentable quiz item" in {
      val quizItem = quiz.findQuizItem(numCorrectAnswersInARowDesired = 0, 
          diffInPromptNum = 0)
      quizItem.isDefined mustEqual true
    }*/
    
    "offer translations for a word, given the group of the word" in { 
      val translations = quiz.findValuesFor(keyWord = "Vertrag", 
          keyType = "German word", valueType = "English word").toSet[String]
      translations.contains("contract") mustEqual true
      translations.contains("treaty") mustEqual true
    }    
    
    "allow key-words to be deleted from a particular group" in {
      val quizLocal = QuizOfWordMappings.fromCustomFormat(quizCustomFormat)
      val quizUpdated = quizLocal.removeWord("full", "English word", "German word")
      val wmg = quizUpdated.findWordMappingGroup(keyType = "English word", 
          valueType = "German word").get
      wmg.contains("full") mustEqual false
    }
    
    "contain unique groups only" in {
      val quizLocal = QuizOfWordMappings.fromCustomFormat(quizCustomFormat)
      quizLocal.numGroups mustEqual 2 // precondition
      val wmg = WordMappingGroupReadWrite("English word", "German word")
      val quizUpdated = quizLocal.addWordMappingGroup(wmg) // should have no effect
      quizUpdated.numGroups mustEqual 2
    }
    
    /*
     * XML on Android is too slow, so a custom format is used    
     *
     * This is not the important performance test. Performance still needs to 
     * tested manually using logging with both the emulator and a real device.
     */
    "deserialize a big quiz quickly" in {
      val fileText = StandardIO.readFile("data/quizGer20k.qui")
      val startParse = System.currentTimeMillis()
      val quiz = QuizOfWordMappings.fromCustomFormat(fileText)        
      val endParse = System.currentTimeMillis()
      println("Time to parse: " + (endParse - startParse))
      endParse - startParse must be<(250L)  // 843
    }
    
  }
}