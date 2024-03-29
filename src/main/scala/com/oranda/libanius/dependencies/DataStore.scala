/*
 * Libanius
 * Copyright (C) 2012-2022 James McCabe <jjtmccabe@gmail.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of tfhe License, or
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

import com.oranda.libanius.io.{DefaultIO, PlatformIO}
import com.oranda.libanius.model.*
import com.oranda.libanius.model.action.serialize.*
import com.oranda.libanius.model.action.serialize.CustomFormat.*
import com.oranda.libanius.model.action.serialize.CustomFormatForModelComponents.*
import com.oranda.libanius.model.quizgroup.*
import com.oranda.libanius.model.wordmapping.Dictionary
import com.oranda.libanius.util.Util

import scala.collection.immutable.Set

trait DataStore extends AppDependencyAccess {
  val io: PlatformIO

  def findQuizGroupHeader(
    promptType: String,
    responseType: String,
    quizGroupType: QuizGroupType
  ): Option[QuizGroupHeader] = {
    val availableQuizGroups = findAvailableQuizGroups
    availableQuizGroups.find(qgh =>
      qgh.promptType == promptType &&
        qgh.responseType == responseType && qgh.quizGroupType == quizGroupType
    )
  }

  def findQuizGroupHeader(promptType: String, responseType: String): Option[QuizGroupHeader] = {
    val availableQuizGroups = findAvailableQuizGroups
    availableQuizGroups.find(qgh =>
      qgh.promptType == promptType
        && qgh.responseType == responseType
    )
  }

  def findQuizGroupHeader(quizGroupKey: QuizGroupKey): Option[QuizGroupHeader] =
    findAvailableQuizGroups.find(_.quizGroupKey == quizGroupKey)

  def loadQuizGroup(header: QuizGroupHeader): QuizGroup = {
    l.log(s"DataStore.loadQuizGroup $header")
    val quizGroup = loadQuizGroupCore(header)
    Util.stopwatch(loadDictionary(header, quizGroup), s"preparing dictionary for $header")
  }

  private def loadDictionary(header: QuizGroupHeader, qg: QuizGroup): QuizGroup = {
    val dictFileName = header.makeDictFileName
    readDictionary(conf.filesDir + dictFileName) match {
      case Some(dictionary) => qg.updatedDictionary(dictionary)
      case _ =>
        l.logError(
          s"Could not find dictionary on filesystem for $header.toString, " +
            "so creating dictionary from the quizGroup"
        )
        qg.updatedDictionary(Dictionary.fromQuizGroup(qg))
    }
  }

  def loadQuizGroupCore(header: QuizGroupHeader): QuizGroup = {
    val quizGroup = findQuizGroupInFilesDir(header) match {
      case Some(qgFileName) =>
        Util.stopwatch(
          readQuizGroupFromFilesDir(qgFileName).getOrElse(QuizGroup()),
          s"reading quiz group from file $qgFileName"
        )
      case _ => initQuizGroup(header)
    }

    if quizGroup.isEmpty then l.logError(s"No quiz items loaded for $header.\nquizGroup loaded was: " + quizGroup)
    quizGroup
  }

  def initQuizGroup(header: QuizGroupHeader): QuizGroup =
    (for
      qgResName <- findQuizGroupInResources(header)
      qgText    <- Util.stopwatch(io.readResource(qgResName), s"reading quiz group resource $qgResName")
    yield qgText) match {
      case Some(qgText) =>
        Util.stopwatch(header.createQuizGroup(qgText), "creating quiz group by parsing text")
      case _ =>
        l.logError(s"failed to load quiz group $header")
        QuizGroup()
    }

  def saveQuiz(quiz: Quiz, path: String = conf.filesDir, userToken: String = ""): Quiz = {

    def saveToFile(header: QuizGroupHeader, quizGroup: QuizGroup, userToken: String) = {
      val fileName   = header.makeQgFileName
      val qgwh       = QuizGroupWithHeader(header, quizGroup)
      val serialized = qgwh.toCustomFormat

      l.log(
        s"Saving quiz group $header.promptType , quiz group has promptNumber " +
          s"quizGroup.currentPromptNumber to $fileName"
      )
      io.writeToFile(path + userToken + "-" + fileName, serialized)
    }
    quiz.activeQuizGroups.foreach { case (header, qg) => saveToFile(header, qg, userToken) }
    quiz
  }

  /*
   * Assumes the Quiz holds a single quiz group -- this is true only for some clients.
   * TODO: should return Stream[String] for Akka Http
   */
  def quizStream(quiz: Quiz): LazyList[Char] =
    quiz.activeQuizGroups.toList.lift(0) match {
      case Some(Tuple2(header, quizGroup)) =>
        val qgwh = QuizGroupWithHeader(header, quizGroup)
        qgwh.toCustomFormat.to(LazyList)
      case None => LazyList.empty
    }

  private def readQuizGroupFromFilesDir(qgFileName: String): Option[QuizGroup] = {
    val qgPath = conf.filesDir + qgFileName
    l.log("reading quiz group from file " + qgPath)
    for qgText <- io.readFile(qgPath)
    yield QuizGroupHeader(qgText).createQuizGroup(qgText)
  }

  private def findQuizGroupInFilesDir(header: QuizGroupHeader): Option[String] = {
    val fileNames = io.findQgFileNamesFromFilesDir
    fileNames.find(io.readQgMetadataFromFile(_) == Option(header))
  }

  private def findQuizGroupInResources(header: QuizGroupHeader): Option[String] = {
    val fileNames = io.findQgFileNamesFromResources
    fileNames.find(io.readQgMetadataFromResource(_) == Option(header))
  }

  def findAvailableQuizGroups: Set[QuizGroupHeader] =
    findAvailableResQuizGroups ++ findAvailableFileQuizGroups

  def loadAllQuizGroupsFromFilesDir: Map[QuizGroupHeader, QuizGroup] = {
    val qgHeaders: Set[QuizGroupHeader] = findAvailableFileQuizGroups
    qgHeaders.map(qgh => Tuple2(qgh, loadQuizGroupCore(qgh))).toMap
  }

  private def findAvailableResQuizGroups: Set[QuizGroupHeader] = {
    val qgResNames = io.findQgFileNamesFromResources
    qgResNames.flatMap(io.readQgMetadataFromResource).toSet
  }

  private def findAvailableFileQuizGroups: Set[QuizGroupHeader] = {
    val qgFileNames = io.findQgFileNamesFromFilesDir
    qgFileNames.flatMap(io.readQgMetadataFromFile).toSet
  }

  // return None if the specified fileName is not found on disk
  private def readDictionary(fileName: String): Option[Dictionary] = {
    val fileText = Util.stopwatch(io.readFile(fileName), "reading dictionary " + fileName)
    val dictionary: Option[Dictionary] =
      Util.stopwatch(fileText.map(deserialize[Dictionary, ParamsNone](_, ParamsNone())), "parsing dictionary")
    val dictionaryNumKeyWords = dictionary.map(_.numKeyWords).getOrElse(0)
    l.log(s"Finished reading $dictionaryNumKeyWords dictionary prompt words")
    dictionary
  }
}

class DataStoreDefault extends DataStore {
  val io = new DefaultIO
}
