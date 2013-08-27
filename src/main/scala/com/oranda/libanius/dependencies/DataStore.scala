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
import com.oranda.libanius.model

case class DataStore(io: PlatformIO) {

  private[this] lazy val l = AppDependencies.logger
  private[this] lazy val conf = AppDependencies.conf

  def readQuizMetadata: Set[model.QuizGroupHeader] = {
    // TODO: consider changing to Platform.readFile
    def readRawMetadata: String =
      io.readFile(conf.fileQuiz).getOrElse(io.readResource(conf.resQuizPublic).get)

    Try(QuizOfWordMappings.metadataFromCustomFormat(readRawMetadata)).recover {
      // for absent data files, security access exceptions or anything else unexpected
      case e: Exception => l.log("Error reading quiz: " + e.getMessage)
                           Set[model.QuizGroupHeader]()
    }.get
  }

  def loadQg(header: model.QuizGroupHeader, loadedQuizGroups: List[WordMappingGroup]):
      Future[WordMappingGroup] = {
    future {
      l.log("seeing if qg was loaded for " + header)
      val loadedQg = loadedQuizGroups.find(_.header == header).getOrElse {
        l.log("no, so loading qg for " + header)
        loadQgCore(header)
      }

      // TODO: move this to a separate Future
      val loadedQgWithDictionary = Util.stopwatch(loadDictionary(loadedQg),
          "preparing dictionary for " + header)

      l.log("loaded qg with numItemsAndCorrectAnswers: " +
          loadedQgWithDictionary.numItemsAndCorrectAnswers +
          " and dictionary with " + loadedQgWithDictionary.dictionary.numKeyWords + " key words")

      loadedQgWithDictionary
    }
  }

  def saveQuiz(quiz: QuizOfWordMappings, path: String = "") {

    def saveToFile(qg: WordMappingGroup) = {
      val saveData = qg.getSaveData
      l.log("Saving qg " + qg.keyType + ", qg has promptNumber " +
          qg.currentPromptNumber + " to " + saveData.fileName)
      io.writeToFile(path + saveData.fileName, saveData.data)
    }
    quiz.wordMappingGroups.foreach(saveToFile(_))
    io.writeToFile(path + conf.fileQuiz, quiz.toCustomFormat.toString)
  }

  private def loadDictionary(qg: WordMappingGroup): WordMappingGroup = {
    val dictFileName = qg.header.makeDictFileName
    val dictionary = readDictionary(dictFileName).getOrElse(
        Dictionary.fromWordMappings(qg.quizPairs))
    qg.updatedDictionary(dictionary)
  }

  def loadQgCore(header: model.QuizGroupHeader): WordMappingGroup =
    findQgInFilesDir(header) match {
      case Some(qgFileName) =>
        Util.stopwatch(readQgFromFilesDir(qgFileName).getOrElse(WordMappingGroup(header)),
            "reading qg from file" + qgFileName)
      case _ =>
        findQgInResources(header) match {
          case Some(qgResName) =>
            val qgText = Util.stopwatch(io.readResource(qgResName).get,
                "reading qg resource " + qgResName)
            io.writeToFile(header.makeQgFileName, qgText) // TODO: eliminate side-effect
            //log("read text from qg resource starting " + qgText.take(200))
            WordMappingGroup.fromCustomFormat(qgText)
          case _ =>
            l.logError("failed to load qg " + header)
            WordMappingGroup(header)
        }
    }

  private def readQgFromFilesDir(qgFileName: String):
      Option[WordMappingGroup] = {
    val qgPath = conf.filesDir + qgFileName
    l.log("reading qg from file " + qgPath)
    for {
      qgText <- io.readFile(qgPath)
      //log("have read qgText " + qgText.take(200) + "... ")
    } yield WordMappingGroup.fromCustomFormat(qgText)
  }

  private def findQgInFilesDir(header: model.QuizGroupHeader): Option[String] = {
    val fileNames = io.findQgFileNamesFromFilesDir
    l.log("fileNames: " + fileNames.toList)
    fileNames.find(io.readQgMetadataFromFile(_) == Some(header))
  }

  private def findQgInResources(header: model.QuizGroupHeader): Option[String] = {
    val fileNames = io.findQgFileNamesFromResources
    l.log("fileNames: " + fileNames.toList)
    fileNames.find(io.readQgMetadataFromResource(_) == Some(header))
  }

  def findAvailableQgs: Set[model.QuizGroupHeader] = findAvailableResQgs ++ findAvailableFileNameQgs

  private def findAvailableResQgs: Set[model.QuizGroupHeader] = {
    val qgResNames = io.findQgFileNamesFromResources
    l.log("qgResNames = " + qgResNames.toList)
    qgResNames.flatMap(io.readQgMetadataFromResource(_)).toSet
  }

  private def findAvailableFileNameQgs: Set[model.QuizGroupHeader] = {
    val qgFileNames = io.findQgFileNamesFromFilesDir
    l.log("qgFileNames = " + qgFileNames.toList)
    qgFileNames.flatMap(io.readQgMetadataFromFile(_)).toSet
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