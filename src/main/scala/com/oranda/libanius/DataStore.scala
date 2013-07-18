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
package com.oranda.libanius

import com.oranda.libanius.model.wordmapping.{WordMappingGroupReadWrite, QuizOfWordMappings, WordMappingGroupReadOnly}
import android.content.Context
import com.oranda.libanius.util.Util
import com.oranda.libanius.io.AndroidIO
import com.oranda.libanius.util.Platform
import scala.collection.immutable.Set

trait DataStore extends Platform {

  def readQuiz(ctx: Context, wordMappingGroups: Set[WordMappingGroupReadWrite]):
      Option[QuizOfWordMappings] = {
    if (wordMappingGroups.isEmpty)
      None
    else
      try {
        val fileText =
          if (ctx.getFileStreamPath(Conf.conf.fileQuiz).exists)
            // TODO: consider changing to Platform.readFile
            Util.stopwatch(AndroidIO.readFile(ctx, Conf.conf.fileQuiz),
                "reading quiz from data/files")
          else
            Util.stopwatch(AndroidIO.readResource(ctx, Conf.conf.resQuizPublic),
                "reading quiz from res/raw")
        Util.stopwatch(Some(QuizOfWordMappings.fromCustomFormat(fileText, wordMappingGroups)),
            "parsing quiz")
      } catch {
        // for absent data files, security access exceptions or anything else unexpected
        case e: Exception => log("Libanius", "Error reading quiz: " + e.getMessage)
                             None
      }
  }

  def readWmgFiles(ctx: Context): Set[WordMappingGroupReadWrite] = {

    // TODO: improve style
    var wmgFiles = ctx.getFilesDir.listFiles.filter(_.getName.endsWith(".wmg")).map(_.getName)
    if (wmgFiles.isEmpty) {
      log("Libanius", "No wmg files found in data dir. Trying resources... ")
      wmgFiles = ctx.getResources.getAssets.list("").filter(_.endsWith("wmg.txt"))
    }
    if (wmgFiles.isEmpty)
      log("Libanius", "No wmg files found at all")

    def readAndParseWmgFile(wmgFile: String) = {
      val wmgFileText = AndroidIO.readFile(ctx, wmgFile)
      WordMappingGroupReadWrite.fromCustomFormat(wmgFileText)
    }

    wmgFiles.map(readAndParseWmgFile(_)).toSet
  }

  def saveQuiz(ctx: Context, quiz: QuizOfWordMappings) {

    def saveToFile(wmg: WordMappingGroupReadWrite) = {
      val saveData = wmg.getSaveData
      log("Libanius", "Saving wmg " + wmg.keyType + ", wmg has promptNumber " +
          wmg.currentPromptNumber)
      AndroidIO.writeToFile(saveData.fileName, saveData.data, ctx)
    }
    quiz.wordMappingGroups.foreach(saveToFile(_))
  }
  
  def readDictionary(ctx: Context): WordMappingGroupReadOnly = {
    val fileText = readDictionaryText(ctx)
    var dictionary = WordMappingGroupReadOnly("", "")
    try {
      dictionary = Util.stopwatch(WordMappingGroupReadOnly.fromCustomFormat(
          fileText), "reading and parsing dictionary")
    } catch {
      case e: Exception =>
          log("Libanius", "Could not parse dictionary: " + e.getMessage(), Some(e))
    }
    log("Libanius", "Finished reading " + dictionary.numKeyWords + " dictionary key words!")
    dictionary
  }
  
  def readDictionaryText(ctx: Context): String = {
    if (ctx.getFileStreamPath(Conf.conf.fileDictionary).exists)
      try {
        // TODO: consider changing to Platform.readFile
        AndroidIO.readFile(ctx, Conf.conf.fileDictionary)
      } catch { 
        // for security access exceptions or anything else unexpected
        case e: Exception => log("Libanius", e.getMessage())
        ""
      }
    else {
      try {
        Util.stopwatch(AndroidIO.readResource(ctx, Conf.conf.resDictPublic),
            "reading quiz from res/raw")
      } catch {
        case e: Exception => log("Libanius", 
            "Could not load dictionary from " + Conf.conf.resDictPublic + "... ")
        ""    
      }   
    }    
  }
}