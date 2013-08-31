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

package com.oranda.libanius.model

import org.specs2.mutable.Specification
import com.oranda.libanius.dependencies.{AppDependencies, Conf}
import com.oranda.libanius.model._

class QuizSpec extends Specification {
  
  "a quiz of word-mappings" should {
    
    val quizData = List(

        "quizGroup type=\"WordMapping\" keyType=\"English word\" valueType=\"German word\" currentPromptNumber=\"0\"\n" +
        "against|wider\n" +
        "entertain|unterhalten\n" +
        "teach|unterrichten\n" +
        "winner|Siegerin\n" +
        "en route|unterwegs\n" +
        "full|satt/voll\n" +
        "interrupted|unterbrochen\n" +
        "contract|Vertrag\n" +
        "rides|reitet\n" +
        "sweeps|streicht\n",

        "quizGroup type=\"WordMapping\" keyType=\"German word\" valueType=\"English word\" currentPromptNumber=\"0\"\n" +
        "unterwegs|en route\n" +
        "Vertrag|contract:697,696;698/treaty:796;798")

    AppDependencies.conf = Conf.setUpForTest()
    
    val quiz = Quiz.demoQuiz(quizData)
    
    "be parseable from custom format" in {
      val qg = quiz.findQuizGroup(QuizGroupHeader(WordMapping, "German word", "English word"))
      qg.isDefined mustEqual true
      //quiz.toCustomFormat.toString mustEqual quizCustomFormat
    }
    
    /* TODO
    "find a presentable quiz item" in {
      val quizItem = quiz.findQuizItem(numCorrectAnswersInARowDesired = 0, 
          diffInPromptNum = 0)
      quizItem.isDefined mustEqual true
    }*/
    
    "offer translations for a word, given the group of the word" in { 
      val translations = quiz.findValuesFor(keyWord = "Vertrag", 
          QuizGroupHeader(WordMapping, "German word", "English word")).toSet[String]
      translations.contains("contract") mustEqual true
      translations.contains("treaty") mustEqual true
    }    
    
    "delete key-words from a particular group" in {
      val quizBefore = Quiz.demoQuiz(quizData)
      val wmgBefore = quizBefore.findQuizGroup(
          QuizGroupHeader(WordMapping, "English word", "German word")).get
      wmgBefore.contains("full") mustEqual true
      val quizAfter = quizBefore.removeKey("full",
          QuizGroupHeader(WordMapping, "English word", "German word"))
      val wmgAfter = quizAfter.findQuizGroup(
          QuizGroupHeader(WordMapping, "English word", "German word")).get
      wmgAfter.contains("full") mustEqual false
    }

    "delete a quiz pair without deleting all values for that key" in {
      val quizBefore = Quiz.demoQuiz(quizData)
      val quizAfter = quizBefore.removeQuizPair(
          keyWord = "Vertrag", QuizValueWithUserAnswers("contract"),
          QuizGroupHeader(WordMapping, "German word", "English word"))
      val translations = quizAfter.findValuesFor(keyWord = "Vertrag",
          QuizGroupHeader(WordMapping, "German word", "English word"))
      translations.contains("contract") mustEqual false
      translations.contains("treaty") mustEqual true
    }

    "contain unique groups only" in {
      val quizLocal = Quiz.demoQuiz(quizData)
      quizLocal.numGroups mustEqual 2 // precondition
      val qg = QuizGroup(QuizGroupHeader(WordMapping, "English word", "German word"))
      val quizUpdated = quizLocal.addQuizGroup(qg) // should have no effect
      quizUpdated.numGroups mustEqual 2
    }
    
    /*
     * XML on Android is too slow, so a custom format is used    
     *
     * This is not the main performance test, just a sanity check. 
     * (Real performance tests can be done manually using logging with both the 
     * emulator and a real device.)
     */
    "deserialize a big quiz quickly" in {
      done
      /* TODO
      val fileText = StandardIO.readFile("data/quizGer20k.qui")
      val startParse = System.currentTimeMillis()
      QuizOfWordMappings.fromCustomFormat(fileText)
      val endParse = System.currentTimeMillis()
      println("Time to parse: " + (endParse - startParse))
      endParse - startParse must be<(2500L)
      */
    }
    
  }
}