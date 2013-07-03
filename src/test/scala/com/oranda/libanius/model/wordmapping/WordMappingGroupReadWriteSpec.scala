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
import com.oranda.libanius.Conf
import com.oranda.libanius.model.UserAnswer

class WordMappingGroupReadWriteSpec extends Specification {
  
  "a word-mapping group" should {
    
    val wmgCustomFormat =
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
        "sweeps|streicht"

    Conf.setUpDummy()
    
    val wmg: WordMappingGroupReadWrite = 
        WordMappingGroupReadWrite.fromCustomFormat(wmgCustomFormat)
    //val wmgFromXml = WordMappingGroupReadWrite.fromXML(wmgXml)
    
    sequential 
    
    "be parseable from custom format" in {
      wmg.keyType mustEqual "English word"
      wmg.valueType mustEqual "German word"
      wmg.toCustomFormat(new StringBuilder()).toString mustEqual wmgCustomFormat
      wmg.numKeyWords mustEqual 10
    }
    "accept the addition of a new word-mapping" in {
      val wmgLocal = WordMappingGroupReadWrite.fromCustomFormat(wmgCustomFormat)
      wmgLocal.contains("good") mustEqual false
      val wmgUpdated = wmgLocal.addWordMapping("good", "gut")
      wmgUpdated.contains("good") mustEqual true
    }
     
    
    "accept new values for an existing word-mapping" in {
      val wmgLocal = WordMappingGroupReadWrite.fromCustomFormat(wmgCustomFormat)
      val valuesForAgainst = wmgLocal.findValuesFor("against")
      valuesForAgainst.isDefined mustEqual true
      valuesForAgainst.get.size mustEqual 1
      val wmgUpdated = wmgLocal.addWordMapping("against", "gegen")
      wmgUpdated.findValuesFor("against").get.size mustEqual 2
    }
    
    "generate false answers similar to a correct answer" in {
      val wmvs = WordMappingValueSet()
      wmvs.addValueToEnd(new WordMappingValue("unterhalten"))
      val falseAnswers = wmg.makeFalseSimilarAnswers(
          wordMappingCorrectValues = wmvs,
          correctValue = new WordMappingValue("unterhalten"), 
          numCorrectAnswersSoFar = 2, numFalseAnswersRequired = 5)
      falseAnswers.contains("unterrichten") mustEqual true
    }

    
    def pullQuizItem(wmg: WordMappingGroupReadWrite) = {
      val quizItem = wmg.findPresentableQuizItem(currentPromptNumber = 0)
      quizItem.isDefined mustEqual true
      // Each time a quiz item is pulled, a user answer must be set
      quizItem.get.wordMappingValue.addUserAnswer(new UserAnswer(true, 0))
      (quizItem.get.keyWord, quizItem.get.wordMappingValue.value)
    }
    
    "add a new word mapping to the front of the its queue" in {
      val wmgLocal = WordMappingGroupReadWrite.fromCustomFormat(wmgCustomFormat)
      val wmgUpdated = wmgLocal.addWordMappingToFront("to exchange", "tauschen")   
      //true mustEqual true
      pullQuizItem(wmgUpdated) mustEqual ("to exchange", "tauschen")       
    }
    
    "move an existing word mapping to the front of its queue" in {
      val wmgLocal = WordMappingGroupReadWrite.fromCustomFormat(wmgCustomFormat)
      val numKeyWordsBefore = wmgLocal.numKeyWords
      val wmgUpdated = wmgLocal.addWordMappingToFront("sweeps", "streicht") 
      val numKeyWordsAfter = wmgUpdated.numKeyWords
      numKeyWordsAfter mustEqual numKeyWordsBefore 
      pullQuizItem(wmgUpdated) mustEqual ("sweeps", "streicht")   
    }
    
    "move a word mapping to the front of its queue where only the key already exists" in {
      val wmgLocal = WordMappingGroupReadWrite.fromCustomFormat(wmgCustomFormat)
      val numKeyWordsBefore = wmgLocal.numKeyWords
      val wmgUpdated = wmgLocal.addWordMappingToFront("entertain", "bewirten") 
      val numKeyWordsAfter = wmgUpdated.numKeyWords
      numKeyWordsAfter mustEqual numKeyWordsBefore      
      pullQuizItem(wmgUpdated) mustEqual ("entertain", "bewirten") 
    }  
    
    "add more than one new word mapping to the front of the its queue" in {
      val wmgLocal = WordMappingGroupReadWrite.fromCustomFormat(wmgCustomFormat)
      val wmgUpdated1 = wmgLocal.addWordMappingToFront("to exchange", "tauschen")
      val wmgUpdated2 = wmgUpdated1.addWordMappingToFront("whole", "ganz")
      pullQuizItem(wmgUpdated2) mustEqual ("whole", "ganz")   
      pullQuizItem(wmgUpdated2) mustEqual ("to exchange", "tauschen")
    }
  }
      
}