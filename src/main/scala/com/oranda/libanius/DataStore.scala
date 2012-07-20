/*
 * Copyright 2012 James McCabe <james@oranda.com>
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
package com.oranda.libanius

import com.oranda.libanius.model.wordmapping.QuizOfWordMappings
import android.content.Context
import com.oranda.libanius.util.Util
import com.oranda.libanius.io.AndroidIO
import com.oranda.libanius.util.Platform
import com.oranda.libanius.model.wordmapping.WordMappingGroupReadOnly

trait DataStore extends Platform {
  
  def readQuiz(ctx: Context): QuizOfWordMappings = {
    val fileText =
      if (ctx.getFileStreamPath(Props.fileQuiz).exists)
        try {
          // TODO: consider changing to Platform.readFile
          Util.stopwatch(AndroidIO.readFile(ctx, Props.fileQuiz), 
              "reading quiz from data/files")
        } catch { 
          // for security access exceptions or anything else unexpected
          case e: Exception => fallBackToDemoQuiz(e.getMessage())
        }
      else {
        try {
          Util.stopwatch(AndroidIO.readResource(ctx, Props.resQuizPublic),
              "reading quiz from res/raw")
        } catch {
          case e: Exception => fallBackToDemoQuiz("Could not load quiz from res/raw")
        }        
      }
    Util.stopwatch(QuizOfWordMappings.fromCustomFormat(fileText), "parsing quiz") 
  }
       
  def fallBackToDemoQuiz(errmsg: String): String = {
    log("Libanius", errmsg + " Using demo data")
    Props.fileQuizRoot = "quizTestData" // for saving
    QuizOfWordMappings.demoDataInCustomFormat
  }
  
  def saveQuiz(ctx: Context, quiz: QuizOfWordMappings) {
    val str = Util.stopwatch(quiz.toCustomFormat, "serialize the quiz")
    AndroidIO.save(ctx, Props.fileQuiz, Props.fileQuizLastBackup, str.toString)
  }
  
  def readDictionary(ctx: Context): WordMappingGroupReadOnly = {
   
    val fileText = readDictionaryText(ctx)
    var dictionary = WordMappingGroupReadOnly("", "")
    try {
      dictionary = Util.stopwatch(WordMappingGroupReadOnly.fromCustomFormat(
        fileText), "reading and parsing dictionary")
    } catch {
      case e: Exception => log("Libanius", 
          "Could not parse dictionary: " + e.getMessage(), e)
    }
    val msg = "Finished reading " + dictionary.numKeyWords + " dictionary key words!"
    log("Libanius", msg)
    dictionary
  }
  
  def readDictionaryText(ctx: Context): String = {
    if (ctx.getFileStreamPath(Props.fileDictionary).exists)
      try {
        // TODO: consider changing to Platform.readFile
        AndroidIO.readFile(ctx, Props.fileDictionary)
      } catch { 
        // for security access exceptions or anything else unexpected
        case e: Exception => log("Libanius", e.getMessage())
        ""
      }
    else {
      try {
        Util.stopwatch(AndroidIO.readResource(ctx, Props.resDictPublic),
            "reading quiz from res/raw")
      } catch {
        case e: Exception => log("Libanius", 
            "Could not load dictionary from res/raw")
        ""    
      }   
    }    
  }
}