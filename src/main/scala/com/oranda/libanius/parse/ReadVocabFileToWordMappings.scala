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

import java.io.File
import scala.io.BufferedSource
import scala.io.Source
import scala.xml.PrettyPrinter
import com.oranda.libanius.io.StandardIO
import com.oranda.libanius.model.wordmapping.QuizOfWordMappings
import com.oranda.libanius.model.wordmapping.WordMappingGroup
import com.oranda.libanius.Props
import com.oranda.libanius.model.wordmapping.WordMappingGroupReadWrite

abstract class ReadVocabFileToWordMappings {
  
  def pathVocab = "data/" + Props.fileVocab
  def pathQuiz = "data/" + Props.fileQuiz
  
  /*
  def readQuizAndVocabFile(): QuizOfWordMappings = {
    var quiz = QuizOfWordMappings(currentPromptNumber = 0) 
     
    if (new File(pathQuiz).exists) {
      val fileText = StandardIO.readFile(pathQuiz)      
      quiz = QuizOfWordMappings.fromXML(xml.XML.loadString(fileText))
    }
    println("key words before vocab file: " + quiz.numKeyWords)
    readVocabFile(quiz)
    println("key words after vocab file: " + quiz.numKeyWords)
    quiz
  }
  */
  def readVocabFile(quiz: QuizOfWordMappings): QuizOfWordMappings = {
    val myFile = new File(pathVocab)
    val src = Source.fromFile(myFile)
    readIntoQuiz(src, quiz)
  }
  
  def readIntoQuiz(src: BufferedSource, quiz: QuizOfWordMappings): QuizOfWordMappings
  
  def addWordMappings(wmGroupEngToGer: WordMappingGroupReadWrite, 
      wmGroupGerToEng: WordMappingGroupReadWrite,
      englishWord: String, germanWord: String) = {
    wmGroupEngToGer.addWordMapping(englishWord, germanWord)
    wmGroupGerToEng.addWordMapping(germanWord, englishWord)
  }
  
  /*
  def readQuiz : QuizOfWordMappings = {
    val fileText = StandardIO.readFile(pathQuiz)      
    QuizOfWordMappings.fromXML(xml.XML.loadString(fileText))
  }
    
  def main(args: Array[String]) {
    println("Reading (quiz and) vocab file...")
    val quiz = readQuizAndVocabFile
    println("Finished reading (quiz and) vocab file... Now writing quiz to " +
        pathQuiz + "...")
    StandardIO.save(pathQuiz, quiz.toCustomFormat.toString)
    println("Finished writing " + quiz.numKeyWords 
        + " words with their translations!")
  }*/
}