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

import com.oranda.libanius.model.wordmapping._
import android.content.Context
import com.oranda.libanius.util.Util
import com.oranda.libanius.io.AndroidIO
import com.oranda.libanius.util.Platform
import scala.collection.immutable.Set
import scala.concurrent.{ future, Future, ExecutionContext }
import ExecutionContext.Implicits.global
import java.io.InputStream
import scala.util.{Failure, Try}

trait DataStore extends Platform {

  def readQuizMetadata(ctx: Context): Set[QuizGroupHeader] = {
    // TODO: consider changing to Platform.readFile
    def readRawMetadata: String = AndroidIO.readFile(ctx, Conf.conf.fileQuiz).getOrElse(
        AndroidIO.readResource(ctx, Conf.conf.resQuizPublic))
    Try(QuizOfWordMappings.metadataFromCustomFormat(readRawMetadata)).recover {
      // for absent data files, security access exceptions or anything else unexpected
      case e: Exception => log("Error reading quiz: " + e.getMessage)
                           Set[QuizGroupHeader]()
    }.get
  }

  def loadWmg(ctx: Context, header: QuizGroupHeader): Future[WordMappingGroup] = {
    log("seeing if wmg was loaded for " + header)
    future {
      val loadedWmg =
        GlobalState.loadedQuizGroups.find(_.header == header).getOrElse {
          log("no, so loading wmg for " + header)
          val loadedWmg = loadWmgCore(ctx, header)
          GlobalState.loadedQuizGroups :+= loadedWmg
          loadedWmg
        }
      log("loadedWmg.numItemsAndCorrectAnswers: " + loadedWmg.numItemsAndCorrectAnswers)

      // TODO: move this to a separate Future
      Util.stopwatch(loadDictionary(ctx, loadedWmg), "preparing dictionary for " + header)

      loadedWmg
    }
  }

  def loadDictionary(ctx: Context, wmg: WordMappingGroup) {
    val dictFileName = wmg.header.makeDictFileName
    val dictionary = readDictionary(ctx, dictFileName).getOrElse(
        Dictionary.fromWordMappings(wmg.wordMappings))
    wmg.setDictionary(dictionary)
  }

  def loadWmgCore(ctx: Context, header: QuizGroupHeader): WordMappingGroup =
    findWmgInFilesDir(ctx, header) match {
      case Some(wmgFileName) =>
        Util.stopwatch(readWmgFromFilesDir(ctx, wmgFileName).getOrElse(WordMappingGroup(header)),
            "reading wmg from file" + wmgFileName)
      case _ =>
        findWmgInResources(ctx, header) match {
          case Some(wmgResName) =>
            val wmgText = Util.stopwatch(AndroidIO.readResource(ctx, wmgResName),
                "reading wmg resource " + wmgResName)
            writeToFile(header.makeWmgFileName, wmgText, Some(ctx)) // TODO: eliminate side-effect
            //log("read text from wmg resource starting " + wmgText.take(200))
            WordMappingGroup.fromCustomFormat(wmgText)
          case _ =>
            logError("failed to load wmg " + header)
            WordMappingGroup(header)
        }
    }

  def readWmgFromFilesDir(ctx: Context, wmgFileName: String): Option[WordMappingGroup] = {
    log("reading wmg from file " + wmgFileName)
    for {
      wmgText <- AndroidIO.readFile(ctx, wmgFileName)
      //log("have read wmgText " + wmgText.take(200) + "... ")
    } yield WordMappingGroup.fromCustomFormat(wmgText)
  }

  def findWmgInFilesDir(ctx: Context, header: QuizGroupHeader): Option[String] = {
    val fileNames = findWmgFileNamesFromFilesDir(ctx)
    log("fileNames: " + fileNames.toList)
    fileNames.find(readWmgMetadataFromFile(ctx, _) == Some(header))
  }

  def findWmgInResources(ctx: Context, header: QuizGroupHeader): Option[String] = {
    val fileNames = findWmgFileNamesFromResources(ctx)
    log("fileNames: " + fileNames.toList)
    fileNames.find(readWmgMetadataFromResource(ctx, _) == Some(header))
  }

  def findAvailableWmgs(ctx: Context): Set[QuizGroupHeader] =
    findAvailableResWmgs(ctx) ++ findAvailableFileNameWmgs(ctx)

  def findAvailableResWmgs(ctx: Context): Set[QuizGroupHeader] = {
    val wmgResNames = findWmgFileNamesFromResources(ctx)
    log("wmgResNames = " + wmgResNames.toList)
    wmgResNames.flatMap(readWmgMetadataFromResource(ctx, _)).toSet
  }

  def findAvailableFileNameWmgs(ctx: Context): Set[QuizGroupHeader] = {
    val wmgFileNames = findWmgFileNamesFromFilesDir(ctx)
    log("wmgFileNames = " + wmgFileNames.toList)
    wmgFileNames.flatMap(readWmgMetadataFromFile(ctx, _)).toSet
  }

  def readWmgMetadataFromFile(ctx: Context, wmgFileName: String): Option[QuizGroupHeader] =
    readWmgMetadata(ctx, AndroidIO.fileToInputStream(wmgFileName))

  def readWmgMetadataFromResource(ctx: Context, wmgResName: String): Option[QuizGroupHeader] =
    readWmgMetadata(ctx, AndroidIO.resourceToInputStream(wmgResName))

  def readWmgMetadata(ctx: Context, inStreamGetter: Context => InputStream):
      Option[QuizGroupHeader] = {

    var firstLine = ""
    (for {
      firstLine <- Try(AndroidIO.readFirstLine(ctx, inStreamGetter))
      wmgMetadata <- Try(Some(QuizGroupHeader(firstLine)))
    } yield wmgMetadata).recover {
      case e: Exception =>
        log("Could not read wmg file, firstLine " + firstLine)
        None
    }.get
  }

  def findWmgFileNamesFromFilesDir(ctx: Context) =
    ctx.getFilesDir.listFiles.filter(_.getName.endsWith(".wmg")).map(_.getName)

  def findWmgFileNamesFromResources(ctx: Context) =
    classOf[R.raw].getFields.map(_.getName).filter(_.startsWith("wmg"))

  def saveWmgs(quiz: QuizOfWordMappings, path: String = "", ctx: Option[Context] = None) {

    def saveToFile(wmg: WordMappingGroup) = {
      val saveData = wmg.getSaveData
      log("Saving wmg " + wmg.keyType + ", wmg has promptNumber " +
          wmg.currentPromptNumber + " to " + saveData.fileName)
      writeToFile(path + saveData.fileName, saveData.data, ctx)
    }
    quiz.wordMappingGroups.foreach(saveToFile(_))
  }

  def readDictionary(ctx: Context, fileName: String): Option[Dictionary] = {
    val fileText = Util.stopwatch(AndroidIO.readFile(ctx, fileName),
        "reading dictionary " + fileName)
    val dictionary = Util.stopwatch(fileText.map(Dictionary.fromCustomFormat(_)),
        "parsing dictionary")
    log("Finished reading " + dictionary.map(_.numKeyWords).getOrElse(0) + " dictionary key words")
    dictionary
  }
}