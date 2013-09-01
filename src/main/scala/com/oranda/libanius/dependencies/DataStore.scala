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

import com.oranda.libanius.util.Util
import scala.collection.immutable.Set
import scala.concurrent.{ future, Future, ExecutionContext }
import ExecutionContext.Implicits.global
import scala.util.Try
import com.oranda.libanius.io.PlatformIO
import com.oranda.libanius.model.wordmapping.Dictionary
import com.oranda.libanius.model.{QuizGroup, Quiz, QuizGroupHeader}

case class DataStore(io: PlatformIO) {

  private[this] lazy val l = AppDependencies.logger
  private[this] lazy val conf = AppDependencies.conf

  def readQuizMetadata: Set[QuizGroupHeader] = {
    // TODO: consider changing to Platform.readFile
    def readRawMetadata: String =
      io.readFile(conf.fileQuiz).getOrElse(io.readResource(conf.resQuizPublic).get)

    Try(Quiz.metadataFromCustomFormat(readRawMetadata)).recover {
      // for absent data files, security access exceptions or anything else unexpected
      case e: Exception => l.log("Error reading quiz: " + e.getMessage)
                           Set[QuizGroupHeader]()
    }.get
  }

  def loadQuizGroup(header: QuizGroupHeader, loadedQuizGroups: List[QuizGroup]):
      Future[QuizGroup] = {
    future {
      l.log("seeing if qg was loaded for " + header)
      val loadedQg = loadedQuizGroups.find(_.header == header).getOrElse {
        l.log("no, so loading qg for " + header)
        loadQuizGroupCore(header)
      }

      // TODO: move this to a separate Future
      val loadedQgWithDictionary = Util.stopwatch(loadDictionary(loadedQg),
          "preparing dictionary for " + header)

      l.log("loaded qg with size " + loadedQgWithDictionary.size +
          " and numCorrectAnswers " + loadedQgWithDictionary.numCorrectAnswers +
          " and dictionary with " + loadedQgWithDictionary.dictionary.numKeyWords + " key words")

      loadedQgWithDictionary
    }
  }

  def saveQuiz(quiz: Quiz, path: String = "") {

    def saveToFile(qg: QuizGroup) = {
      val saveData = qg.getSaveData
      l.log("Saving qg " + qg.promptType + ", qg has promptNumber " +
          qg.currentPromptNumber + " to " + saveData.fileName)
      io.writeToFile(path + saveData.fileName, saveData.data)
    }
    quiz.quizGroups.foreach(saveToFile(_))
    io.writeToFile(path + conf.fileQuiz, quiz.toCustomFormat.toString)
  }

  private def loadDictionary(qg: QuizGroup): QuizGroup = {
    val dictFileName = qg.header.makeDictFileName
    readDictionary(dictFileName) match {
      case Some(dictionary) => qg.updatedDictionary(dictionary)
      case _ => qg
    }
  }

  def loadQuizGroupCore(header: QuizGroupHeader): QuizGroup =
    findQuizGroupInFilesDir(header) match {
      case Some(qgFileName) =>
        Util.stopwatch(readQuizGroupFromFilesDir(qgFileName).getOrElse(QuizGroup(header)),
            "reading qg from file" + qgFileName)
      case _ =>
        findQuizGroupInResources(header) match {
          case Some(qgResName) =>
            val qgText = Util.stopwatch(io.readResource(qgResName).get,
                "reading qg resource " + qgResName)
            //log("read text from qg resource starting " + qgText.take(200))
            header.createQuizGroup(qgText)
          case _ =>
            l.logError("failed to load qg " + header)
            QuizGroup(header)
        }
    }

  private def readQuizGroupFromFilesDir(qgFileName: String):
      Option[QuizGroup] = {
    val qgPath = conf.filesDir + qgFileName
    l.log("reading qg from file " + qgPath)
    for {
      qgText <- io.readFile(qgPath)
      //log("have read qgText " + qgText.take(200) + "... ")
    } yield QuizGroupHeader(qgText).createQuizGroup(qgText)
  }

  private def findQuizGroupInFilesDir(header: QuizGroupHeader): Option[String] = {
    val fileNames = io.findQgFileNamesFromFilesDir
    l.log("fileNames: " + fileNames.toList)
    fileNames.find(io.readQgMetadataFromFile(_) == Some(header))
  }

  private def findQuizGroupInResources(header: QuizGroupHeader): Option[String] = {
    val fileNames = io.findQgFileNamesFromResources
    l.log("fileNames: " + fileNames.toList)
    fileNames.find(io.readQgMetadataFromResource(_) == Some(header))
  }

  def findAvailableQuizGroups: Set[QuizGroupHeader] = {
    l.log("res groups " + findAvailableResQuizGroups)
    l.log("fileName groups " + findAvailableFileNameQuizGroups)
    findAvailableResQuizGroups ++ findAvailableFileNameQuizGroups
  }


  private def findAvailableResQuizGroups: Set[QuizGroupHeader] = {
    val qgResNames = io.findQgFileNamesFromResources
    l.log("qgResNames = " + qgResNames.toList)
    qgResNames.flatMap(io.readQgMetadataFromResource(_)).toSet
  }

  private def findAvailableFileNameQuizGroups: Set[QuizGroupHeader] = {
    val qgFileNames = io.findQgFileNamesFromFilesDir
    l.log("qgFileNames = " + qgFileNames.toList)
    qgFileNames.flatMap(io.readQgMetadataFromFile(_)).toSet
  }

  // return None if the specified fileName is not found on disk
  private def readDictionary(fileName: String): Option[Dictionary] = {
    val fileText = Util.stopwatch(io.readFile(fileName), "reading dictionary " + fileName)
    val dictionary = Util.stopwatch(fileText.map(Dictionary.fromCustomFormat(_)),
        "parsing dictionary")
    l.log("Finished reading " + dictionary.map(_.numKeyWords).getOrElse(0) + " dictionary prompt words")
    dictionary
  }
}