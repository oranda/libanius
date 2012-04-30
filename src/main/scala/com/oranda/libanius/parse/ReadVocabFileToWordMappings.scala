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

package com.oranda.libanius.parse

import java.io.File

import scala.io.BufferedSource
import scala.io.Source
import scala.xml.PrettyPrinter

import com.oranda.libanius.io.StandardIO
import com.oranda.libanius.model.wordmapping.QuizOfWordMappings
import com.oranda.libanius.model.wordmapping.WordMappingGroup
import com.oranda.libanius.Props

abstract class ReadVocabFileToWordMappings {
  
  def getVocabFilename = "data/" + Props.fileVocab
  def getQuizFilename = "data/" + Props.fileQuiz
  
  def readQuizAndVocabFile(): QuizOfWordMappings = {
    var quiz = new QuizOfWordMappings(_currentPromptNumber = 0) 
     
    if (new File(getQuizFilename).exists) {
      val fileText = StandardIO.readFile(getQuizFilename)      
      quiz = QuizOfWordMappings.fromXML(xml.XML.loadString(fileText))
    }
    println("key words before vocab file: " + quiz.numKeyWords)
    readVocabFile(quiz)
    println("key words after vocab file: " + quiz.numKeyWords)
    quiz
  }
  
  def readVocabFile(quiz: QuizOfWordMappings): QuizOfWordMappings = {
    val myFile = new File(getVocabFilename)
    val src = Source.fromFile(myFile)
    return readIntoQuiz(src, quiz)
  }
  
  def readIntoQuiz(src: BufferedSource, quiz: QuizOfWordMappings): QuizOfWordMappings
  
  def addWordMappings(wmGroupEngToGer: WordMappingGroup, 
      wmGroupGerToEng: WordMappingGroup,
      englishWord: String, germanWord: String) = {
    wmGroupEngToGer.addWordMapping(englishWord, germanWord)
    wmGroupGerToEng.addWordMapping(germanWord, englishWord)
  }
  
  def readQuiz : QuizOfWordMappings = {
    val fileText = StandardIO.readFile(Props.fileQuiz)      
    QuizOfWordMappings.fromXML(xml.XML.loadString(fileText))
  }
    
  def main(args: Array[String]) {
    println("Reading (quiz and) vocab file...")
    val quiz = readQuizAndVocabFile
    println("Finished reading (quiz and) vocab file... Now writing quiz...")
    StandardIO.save(getQuizFilename, new PrettyPrinter(999, 2).format(quiz.toXML))
    println("Finished writing " + quiz.numKeyWords 
        + " words with their translations!")
  }
  
    /*
    def processLine(line: String) : Boolean = {
    	val r = ";"
      	val items = line.split(r)
    	if (items.length > 4) {
    		println(items(0) + ": " + items(4));
    		return true;
    	}
    	return false;
    }
    */
}