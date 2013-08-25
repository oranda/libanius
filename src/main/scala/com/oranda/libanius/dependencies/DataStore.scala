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

package com.oranda.libanius.dependencies

import com.oranda.libanius.model.wordmapping._
import com.oranda.libanius.util.Util
import scala.collection.immutable.Set
import scala.concurrent.{ future, Future, ExecutionContext }
import ExecutionContext.Implicits.global
import scala.util.Try
import com.oranda.libanius.io.PlatformIO

case class DataStore(io: PlatformIO) {

  private[this] lazy val l = AppDependencies.logger
  private[this] lazy val conf = AppDependencies.conf

  def readQuizMetadata: Set[QuizGroupHeader] = {
    // TODO: consider changing to Platform.readFile
    def readRawMetadata: String =
      io.readFile(conf.fileQuiz).getOrElse(io.readResource(conf.resQuizPublic).get)

    Try(QuizOfWordMappings.metadataFromCustomFormat(readRawMetadata)).recover {
      // for absent data files, security access exceptions or anything else unexpected
      case e: Exception => l.log("Error reading quiz: " + e.getMessage)
                           Set[QuizGroupHeader]()
    }.get
  }

  def loadWmg(header: QuizGroupHeader, loadedQuizGroups: List[WordMappingGroup]):
      Future[WordMappingGroup] = {
    future {
      l.log("seeing if wmg was loaded for " + header)
      val loadedWmg = loadedQuizGroups.find(_.header == header).getOrElse {
        l.log("no, so loading wmg for " + header)
        loadWmgCore(header)
      }

      // TODO: move this to a separate Future
      val loadedWmgWithDictionary = Util.stopwatch(loadDictionary(loadedWmg),
          "preparing dictionary for " + header)

      l.log("loaded wmg with numItemsAndCorrectAnswers: " +
          loadedWmgWithDictionary.numItemsAndCorrectAnswers +
          " and dictionary with " + loadedWmgWithDictionary.dictionary.numKeyWords + " key words")

      loadedWmgWithDictionary
    }
  }

  def saveQuiz(quiz: QuizOfWordMappings, path: String = "") {

    def saveToFile(wmg: WordMappingGroup) = {
      val saveData = wmg.getSaveData
      l.log("Saving wmg " + wmg.keyType + ", wmg has promptNumber " +
          wmg.currentPromptNumber + " to " + saveData.fileName)
      io.writeToFile(path + saveData.fileName, saveData.data)
    }
    quiz.wordMappingGroups.foreach(saveToFile(_))
    io.writeToFile(path + conf.fileQuiz, quiz.toCustomFormat.toString)
  }

  private def loadDictionary(wmg: WordMappingGroup): WordMappingGroup = {
    val dictFileName = wmg.header.makeDictFileName
    val dictionary = readDictionary(dictFileName).getOrElse(
        Dictionary.fromWordMappings(wmg.wordMappings))
    wmg.updatedDictionary(dictionary)
  }

  def loadWmgCore(header: QuizGroupHeader): WordMappingGroup =
    findWmgInFilesDir(header) match {
      case Some(wmgFileName) =>
        Util.stopwatch(readWmgFromFilesDir(wmgFileName).getOrElse(WordMappingGroup(header)),
            "reading wmg from file" + wmgFileName)
      case _ =>
        findWmgInResources(header) match {
          case Some(wmgResName) =>
            val wmgText = Util.stopwatch(io.readResource(wmgResName).get,
                "reading wmg resource " + wmgResName)
            io.writeToFile(header.makeWmgFileName, wmgText) // TODO: eliminate side-effect
            //log("read text from wmg resource starting " + wmgText.take(200))
            WordMappingGroup.fromCustomFormat(wmgText)
          case _ =>
            l.logError("failed to load wmg " + header)
            WordMappingGroup(header)
        }
    }

  private def readWmgFromFilesDir(wmgFileName: String):
      Option[WordMappingGroup] = {
    val wmgPath = conf.filesDir + wmgFileName
    l.log("reading wmg from file " + wmgPath)
    for {
      wmgText <- io.readFile(wmgPath)
      //log("have read wmgText " + wmgText.take(200) + "... ")
    } yield WordMappingGroup.fromCustomFormat(wmgText)
  }

  private def findWmgInFilesDir(header: QuizGroupHeader): Option[String] = {
    val fileNames = io.findWmgFileNamesFromFilesDir
    l.log("fileNames: " + fileNames.toList)
    fileNames.find(io.readWmgMetadataFromFile(_) == Some(header))
  }

  private def findWmgInResources(header: QuizGroupHeader): Option[String] = {
    val fileNames = io.findWmgFileNamesFromResources
    l.log("fileNames: " + fileNames.toList)
    fileNames.find(io.readWmgMetadataFromResource(_) == Some(header))
  }

  def findAvailableWmgs: Set[QuizGroupHeader] = findAvailableResWmgs ++ findAvailableFileNameWmgs

  private def findAvailableResWmgs: Set[QuizGroupHeader] = {
    val wmgResNames = io.findWmgFileNamesFromResources
    l.log("wmgResNames = " + wmgResNames.toList)
    wmgResNames.flatMap(io.readWmgMetadataFromResource(_)).toSet
  }

  private def findAvailableFileNameWmgs: Set[QuizGroupHeader] = {
    val wmgFileNames = io.findWmgFileNamesFromFilesDir
    l.log("wmgFileNames = " + wmgFileNames.toList)
    wmgFileNames.flatMap(io.readWmgMetadataFromFile(_)).toSet
  }

  // return None if the specified fileName is not found on disk
  private def readDictionary(fileName: String): Option[Dictionary] = {
    val fileText = Util.stopwatch(io.readFile(fileName), "reading dictionary " + fileName)
    val dictionary = Util.stopwatch(fileText.map(Dictionary.fromCustomFormat(_)),
        "parsing dictionary")
    l.log("Finished reading " + dictionary.map(_.numKeyWords).getOrElse(0) + " dictionary key words")
    dictionary
  }
}