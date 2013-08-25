/* Copyright 2012-2013 James McCabe <james@oranda.com>
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
import com.oranda.libanius.model.UserAnswer
import com.oranda.libanius.dependencies.{AppDependencies, Conf}

class WordMappingGroupSpec extends Specification {
  
  "a word-mapping group" should {

    val wmgCustomFormat =
        "wordMappingGroup keyType=\"English word\" valueType=\"German word\" currentPromptNumber=\"0\"\n" +
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

    AppDependencies.conf = Conf.setUpForTest()

    val wmg = WordMappingGroup.fromCustomFormat(wmgCustomFormat)
    
    sequential

    "be parseable from custom format" in {
      wmg.currentPromptNumber mustEqual 0
      wmg.keyType mustEqual "English word"
      wmg.valueType mustEqual "German word"
      wmg.toCustomFormat(new StringBuilder()).toString mustEqual wmgCustomFormat
      wmg.numKeyWords mustEqual 10
    }

    "accept the addition of a new word-mapping" in {
      val wmgLocal = WordMappingGroup.fromCustomFormat(wmgCustomFormat)
      wmgLocal.contains("good") mustEqual false
      val wmgUpdated = wmgLocal.addWordMapping("good", "gut")
      wmgUpdated.contains("good") mustEqual true
    }

    "accept new values for an existing word-mapping" in {
      val wmgLocal = WordMappingGroup.fromCustomFormat(wmgCustomFormat)
      val valuesForAgainst = wmgLocal.findValueSetFor("against")
      valuesForAgainst.isDefined mustEqual true
      valuesForAgainst.get.size mustEqual 1
      val wmgUpdated = wmgLocal.addWordMapping("against", "gegen")
      wmgUpdated.findValueSetFor("against").get.size mustEqual 2
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

    def pullQuizItem(wmg: WordMappingGroup): (WordMappingGroup, (String, String)) = {
      val quizItem = wmg.findPresentableQuizItem
      quizItem.isDefined mustEqual true
      // Each time a quiz item is pulled, a user answer must be set
      val wmgUpdated = wmg.updateWithUserAnswer(quizItem.get.keyWord,
          quizItem.get.wmvs, quizItem.get.wordMappingValue, new UserAnswer(true, 0))
      (wmgUpdated, (quizItem.get.keyWord, quizItem.get.wordMappingValue.value))
    }

    "find a presentable quiz item" in {
      //println("find a presentable quiz item: wmg.wordMappings: " + wmg.wordMappings)
      pullQuizItem(wmg)._2 mustEqual ("against", "wider")
    }

    "add a new word mapping to the front of its queue" in {
      val wmgLocal = WordMappingGroup.fromCustomFormat(wmgCustomFormat)
      val wmgUpdated = wmgLocal.addWordMappingToFront("to exchange", "tauschen")
      pullQuizItem(wmgUpdated)._2 mustEqual ("to exchange", "tauschen")
    }
    
    "move an existing word mapping to the front of its queue" in {
      val wmgLocal = WordMappingGroup.fromCustomFormat(wmgCustomFormat)
      val numKeyWordsBefore = wmgLocal.numKeyWords
      val wmgUpdated = wmgLocal.addWordMappingToFront("sweeps", "streicht") 
      val numKeyWordsAfter = wmgUpdated.numKeyWords
      numKeyWordsAfter mustEqual numKeyWordsBefore 
      pullQuizItem(wmgUpdated)._2 mustEqual ("sweeps", "streicht")
    }
    
    "move a word mapping to the front of its queue where only the key already exists" in {
      val wmgLocal = WordMappingGroup.fromCustomFormat(wmgCustomFormat)
      val numKeyWordsBefore = wmgLocal.numKeyWords
      val wmgUpdated = wmgLocal.addWordMappingToFront("entertain", "bewirten") 
      val numKeyWordsAfter = wmgUpdated.numKeyWords
      numKeyWordsAfter mustEqual numKeyWordsBefore      
      pullQuizItem(wmgUpdated)._2 mustEqual ("entertain", "bewirten")
    }  
    
    "add more than one new word mapping to the front of the its queue" in {
      val wmgLocal = WordMappingGroup.fromCustomFormat(wmgCustomFormat)
      val wmgUpdated1 = wmgLocal.addWordMappingToFront("to exchange", "tauschen")
      val wmgUpdated2 = wmgUpdated1.addWordMappingToFront("whole", "ganz")
      val (wmgUnrolled, (keyWord, value)) = pullQuizItem(wmgUpdated2)
      (keyWord, value) mustEqual ("whole", "ganz")
      pullQuizItem(wmgUnrolled)._2 mustEqual ("to exchange", "tauschen")
    }

    def pullQuizItemAndAnswerCorrectly(wmg: WordMappingGroup): WordMappingGroup = {
      val quizItem = wmg.findPresentableQuizItem.get
      updateWithUserAnswer(wmg, quizItem)
    }

    def updateWithUserAnswer(wmg: WordMappingGroup, quizItem: QuizItemViewWithChoices) = {
      val userAnswer = new UserAnswer(true, wmg.currentPromptNumber)
      wmg.updateWithUserAnswer(quizItem.keyWord, quizItem.wmvs,
          quizItem.wordMappingValue, userAnswer).updatedPromptNumber
    }

    "should present an item that has been answered before after five prompts" in {
      var wmgLocal = WordMappingGroup.fromCustomFormat(wmgCustomFormat)
      val quizItem0 = wmgLocal.findPresentableQuizItem.get
      quizItem0.wordMappingValue.value mustEqual "wider"
      wmgLocal = updateWithUserAnswer(wmgLocal, quizItem0)

      for (promptNum <- 1 until 5)
        wmgLocal = pullQuizItemAndAnswerCorrectly(wmgLocal)

      val quizItem5 = wmgLocal.findPresentableQuizItem
      quizItem5.get.wordMappingValue.value mustEqual "wider"
    }
  }
      
}