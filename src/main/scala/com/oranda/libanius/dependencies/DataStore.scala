/*
 * Libanius
 * Copyright (C) 2012-2015 James McCabe <james@oranda.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.oranda.libanius.dependencies

import com.oranda.libanius.model._
import com.oranda.libanius.model.action.serialize._
import CustomFormat._
import CustomFormatForModelComponents._
import com.oranda.libanius.util.Util
import scala.collection.immutable.Set
import scala.util.Try
import com.oranda.libanius.io.{DefaultIO, PlatformIO}
import com.oranda.libanius.model.wordmapping.Dictionary
import com.oranda.libanius.model.quizgroup.{QuizGroupWithHeader, QuizGroupHeader, QuizGroup, QuizGroupType}

class DataStore(io: PlatformIO) extends AppDependencyAccess {

  def readQuizMetadata: Set[QuizGroupHeader] = {
    def readRawMetadata: String =
      io.readFile(conf.fileQuiz).getOrElse(io.readResource(conf.resQuizPublic).get)

    Try(Quiz.metadataFromCustomFormat(readRawMetadata)).recover {
      // for absent data files, security access exceptions or anything else unexpected
      case e: Exception => l.log("Error reading quiz: " + e.getMessage)
                           Set[QuizGroupHeader]()
    }.get
  }

  def findQuizGroupHeader(promptType: String, responseType: String, quizGroupType: QuizGroupType):
      Option[QuizGroupHeader] = {
    val availableQuizGroups = findAvailableQuizGroups
    availableQuizGroups.find(qgh => qgh.promptType == promptType &&
        qgh.responseType == responseType && qgh.quizGroupType == quizGroupType)
  }

  def findQuizGroupHeader(promptType: String, responseType: String): Option[QuizGroupHeader] = {
    val availableQuizGroups = findAvailableQuizGroups
    availableQuizGroups.find(qgh => qgh.promptType == promptType &&
        qgh.responseType == responseType)
  }

  def loadQuizGroup(header: QuizGroupHeader): QuizGroup = {
    l.log("DataStore.loadQuizGroup " + header)
    val quizGroup = loadQuizGroupCore(header)
    Util.stopwatch(loadDictionary(header, quizGroup), "preparing dictionary for " + header)
  }

  private def loadDictionary(header: QuizGroupHeader, qg: QuizGroup): QuizGroup = {
    val dictFileName = header.makeDictFileName
    readDictionary(conf.filesDir + dictFileName) match {
      case Some(dictionary) => qg.updatedDictionary(dictionary)
      case _ => l.logError("Could not find dictionary on filesystem for " +
              header.toString + ", so creating dictionary from the quizGroup")
          qg.updatedDictionary(Dictionary.fromQuizGroup(qg))
    }
  }

  def loadQuizGroupCore(header: QuizGroupHeader): QuizGroup = {

    val quizGroup = findQuizGroupInFilesDir(header) match {
      case Some(qgFileName) =>
        Util.stopwatch(readQuizGroupFromFilesDir(qgFileName).getOrElse(QuizGroup()),
            "reading quiz group from file " + qgFileName)
      case _ => initQuizGroup(header)
    }

    if (quizGroup.isEmpty) l.logError("No quiz items loaded for " + header)
    quizGroup
  }

  def initQuizGroup(header: QuizGroupHeader): QuizGroup = {
    findQuizGroupInResources(header) match {
      case Some(qgResName) =>
        val qgText = Util.stopwatch(io.readResource(qgResName).get,
          "reading quiz group resource " + qgResName)
        Util.stopwatch(header.createQuizGroup(qgText), "creating quiz group by parsing text")
      case _ =>
        l.logError("failed to load quiz group " + header)
        QuizGroup()
    }
  }

  def saveQuiz(quiz: Quiz, path: String = "", userToken: String = "") {

    def saveToFile(header: QuizGroupHeader, quizGroup: QuizGroup, userToken: String) = {
      val fileName = header.makeQgFileName
      val qgwh = QuizGroupWithHeader(header, quizGroup)
      val serialized = qgwh.toCustomFormat

      l.log("Saving quiz group " + header.promptType + ", quiz group has promptNumber " +
          quizGroup.currentPromptNumber + " to " + fileName)
      io.writeToFile(path + userToken + "-" + fileName, serialized)
    }
    quiz.activeQuizGroups.foreach { case (header, qg) => saveToFile(header, qg, userToken) }
  }

  private def readQuizGroupFromFilesDir(qgFileName: String):
      Option[QuizGroup] = {
    val qgPath = conf.filesDir + qgFileName
    l.log("reading quiz group from file " + qgPath)
    for {
      qgText <- io.readFile(qgPath)
    } yield QuizGroupHeader(qgText).createQuizGroup(qgText)
  }

  private def findQuizGroupInFilesDir(header: QuizGroupHeader): Option[String] = {
    val fileNames = io.findQgFileNamesFromFilesDir
    fileNames.find(io.readQgMetadataFromFile(_) == Some(header))
  }

  private def findQuizGroupInResources(header: QuizGroupHeader): Option[String] = {
    val fileNames = io.findQgFileNamesFromResources
    fileNames.find(io.readQgMetadataFromResource(_) == Some(header))
  }

  def findAvailableQuizGroups: Set[QuizGroupHeader] =
    findAvailableResQuizGroups ++ findAvailableFileQuizGroups

  def loadAllQuizGroupsFromFilesDir: Map[QuizGroupHeader, QuizGroup] = {
    val qgHeaders: Set[QuizGroupHeader] = findAvailableFileQuizGroups
    qgHeaders.map(qgh => Tuple2(qgh, loadQuizGroupCore(qgh))).toMap
  }

  private def findAvailableResQuizGroups: Set[QuizGroupHeader] = {
    val qgResNames = io.findQgFileNamesFromResources
    qgResNames.flatMap(io.readQgMetadataFromResource(_)).toSet
  }

  private def findAvailableFileQuizGroups: Set[QuizGroupHeader] = {
    val qgFileNames = io.findQgFileNamesFromFilesDir
    qgFileNames.flatMap(io.readQgMetadataFromFile(_)).toSet
  }

  // return None if the specified fileName is not found on disk
  private def readDictionary(fileName: String): Option[Dictionary] = {
    val fileText = Util.stopwatch(io.readFile(fileName), "reading dictionary " + fileName)
    val dictionary: Option[Dictionary] =
        Util.stopwatch(fileText.map(
            deserialize[Dictionary, NoParams](_, NoParams())),
            "parsing dictionary")
    l.log("Finished reading " + dictionary.map(_.numKeyWords).getOrElse(0) +
        " dictionary prompt words")
    dictionary
  }
}

class DataStoreDefault(io: PlatformIO) extends DataStore(new DefaultIO)