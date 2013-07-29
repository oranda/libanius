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

import com.oranda.libanius.model.wordmapping._
import scala.collection.immutable.Stream

abstract class ReadVocabFileTwoLists extends ReadVocabFileToWordMappings {

  var englishMode = false
  val englishWords = ListBuffer[String]()
  val otherWords = ListBuffer[String]()

  def readIntoQuiz(src: BufferedSource, quiz: QuizOfWordMappings,
      keyType: String, valueType: String): QuizOfWordMappings = {
    
    src.getLines.foreach(line => readQuizItemFrequency(line.trim))
    println(keyType + "s length: " + otherWords.size)
    println(valueType + "s length: " + englishWords.size)
    val wordsOtherToEnglish = otherWords zip englishWords
    val wordsEnglishToOther = englishWords zip otherWords

    val wmg1 = makeWmg(wordsOtherToEnglish, keyType, valueType)
    val wmg2 = makeWmg(wordsEnglishToOther, valueType, keyType)
    val quizUpdated = quiz.addWordMappingGroup(wmg1).addWordMappingGroup(wmg2)

    println("Number of wmgs in quiz: " + quizUpdated.wordMappingGroups.size)
    quizUpdated
  }

  def makeWmg(combinedWords: Seq[(String, String)], type1: String, type2: String) = {
    val wordMappings = combinedWords.map(keyValue =>
        (keyValue._1, WordMappingValueSetWrapper(List(WordMappingValue(keyValue._2)))))
    WordMappingGroupReadWrite(QuizGroupHeader(type1, type2), wordMappings.toStream)
  }
  
  def readQuizItemFrequency(word: String) =
    word match {
      case w if w.startsWith("--") => englishMode = true
      case _ => if (englishMode) englishWords += word else otherWords += word
    }
}

object ReadVocabTwoListsGermanEnglish extends ReadVocabFileTwoLists {

  val fileVocab = "vocabGer10k.txt"

  override def readIntoQuiz(src: BufferedSource, quiz: QuizOfWordMappings): QuizOfWordMappings =
    readIntoQuiz(src, quiz, "German word", "English word")
}

object ReadVocabTwoListsSpanishEnglish extends ReadVocabFileTwoLists {

  val fileVocab = "vocabSpan10k.txt"

  override def readIntoQuiz(src: BufferedSource, quiz: QuizOfWordMappings): QuizOfWordMappings =
    readIntoQuiz(src, quiz, "Spanish word", "English word")

}