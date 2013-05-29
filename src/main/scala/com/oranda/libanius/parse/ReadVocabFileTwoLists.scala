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

import scala.io.BufferedSource
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.LinkedHashSet

import com.oranda.libanius.model.wordmapping.QuizOfWordMappings
import com.oranda.libanius.model.wordmapping.WordMappingGroup

object ReadVocabTwoLists extends ReadVocabFileToWordMappings {
  
  var germanMode = true
  val englishWords = ListBuffer[String]()
  val germanWords = ListBuffer[String]()
    
  override def readIntoQuiz(src: BufferedSource, quiz: QuizOfWordMappings): 
      QuizOfWordMappings = {

    val wmGroupEngToGer = quiz.findOrAddWordMappingGroup(
        keyType="English word", valueType="German word")
    val wmGroupGerToEng = quiz.findOrAddWordMappingGroup(
        keyType="German word", valueType="English word")
    
    src.getLines.foreach(line => readQuizItemFrequency(line.trim))
    println("germanWords length: " + germanWords.size)
    println("englishWords length: " + englishWords.size)
    val combinedWords = germanWords zip englishWords
    combinedWords.foreach(pair => addWordMappings(
        wmGroupEngToGer, wmGroupGerToEng, pair._2, pair._1))
    quiz
  }
  
  def readQuizItemFrequency(word: String) = {
    word match {
      case w if w.startsWith("--") =>
        germanMode = false
      case _ =>
        if (germanMode)
          germanWords += word
        else
          englishWords += word
    }
  }

}