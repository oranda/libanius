/*
 * Libanius
 * Copyright (C) 2012-2020 James McCabe <jjtmccabe@gmail.com>
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

package com.oranda.libanius.io

import java.io._
import scala.io.Source
import scala.language.reflectiveCalls
import com.oranda.libanius.model.quizgroup.QuizGroupHeader


/*
 * Assumes an ordinary Unix/Windows filesystem.
 */
class DefaultIO extends PlatformIO {

  def readFile(fileName: String): Option[String] = {
    val theFile = new File(fileName)
    if (theFile.exists) {
      val fileStream = Source.fromFile(theFile)
      val str = fileStream.mkString
      fileStream.close()
      Option(str)
    } else {
      l.logError(s"File not found: $fileName")
      None
    }
  }

  def save(fileName: String, strToSave: String): Unit =
    writeToFile(fileName, strToSave)

  def writeToFile(fileName: String, data: String) =
    using (new FileWriter(fileName)) (_.write(data))

  private def using[A <: {def close(): Unit}, B](param: A)(f: A => B): B =
	  try { f(param) } finally { param.close() }

  def readResource(resName: String): Option[String] =
    readFile(conf.resourcesDir + resName)

  override def readQgMetadataFromFile(qgFileName: String): Option[QuizGroupHeader] =
    readQgMetadata(conf.filesDir + qgFileName)

  override def readQgMetadataFromResource(qgResName: String): Option[QuizGroupHeader] =
    readQgMetadata(conf.resourcesDir + qgResName)

  private def readQgMetadata(qgPath: String): Option[QuizGroupHeader] = {
    val file = new File(qgPath)
    if (file.exists) {
      l.log(s"readQgMetadata: reading file $qgPath")
      readQgMetadata(new FileInputStream(file))
    } else {
      l.logError(s"File not found: $qgPath")
      None
    }
  }

  override def findQgFileNamesFromFilesDir =
    new File(conf.filesDir).listFiles.filter(_.getName.endsWith(".qgr")).map(_.getName)

  override def findQgFileNamesFromResources =
    new File(conf.resourcesDir).listFiles.filter(_.getName.startsWith("qgr")).map(_.getName)
}
