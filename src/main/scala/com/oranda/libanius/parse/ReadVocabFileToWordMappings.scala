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
import com.oranda.libanius.io.DefaultIO
import com.oranda.libanius.dependencies._
import com.oranda.libanius.model.Quiz

abstract class ReadVocabFileToWordMappings extends AppDependencyAccess {

  def fileVocab: String
  def pathVocab = "data/" + fileVocab
  def pathQuiz = "data/" + conf.fileQuiz

  /*
  def readQuizAndVocabFile: QuizOfWordMappings = {
    var quiz = QuizOfWordMappings(currentPromptNumber = 0) 
     
    if (new File(pathQuiz).exists) {
      val fileText = StandardIO.readFile(pathQuiz)      
      quiz = QuizOfWordMappings.fromXML(xml.XML.loadString(fileText))
    }
    println("prompt words before vocab file: " + quiz.numKeyWords)
    readVocabFile(quiz)
    println("prompt words after vocab file: " + quiz.numKeyWords)
    quiz
  } */

  def readVocabFile(quiz: Quiz): Quiz = {
    val myFile = new File(pathVocab)
    val src = Source.fromFile(myFile)
    readIntoQuiz(src, quiz)
  }
  
  def readIntoQuiz(src: BufferedSource, quiz: Quiz): Quiz

  /*
  def readQuiz : QuizOfWordMappings = {
    val fileText = StandardIO.readFile(pathQuiz)      
    QuizOfWordMappings.fromXML(xml.XML.loadString(fileText))
  }
  */
  def main(args: Array[String]) {
    Conf.setUpForParsing("quizSpan10k")
    println("Reading vocab file...")
    val quiz = readVocabFile(new Quiz())
    println("quiz first wmg length is " + quiz.quizGroups.toList(0).quizPrompts.length)
    println("Finished reading (quiz and) vocab file... Now writing quiz to " + pathQuiz + "...")
    new DefaultIO().save(pathQuiz, quiz.toCustomFormat.toString)
    dataStore.saveQuiz(quiz, path = "data/")
    println("Finished writing " + quiz.numPrompts + " words with their translations!")
  }
}