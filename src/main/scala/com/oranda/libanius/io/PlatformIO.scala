/*
 * Libanius
 * Copyright (C) 2012-2022 James McCabe <jjtmccabe@gmail.com>
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

import com.oranda.libanius.dependencies.*
import com.oranda.libanius.model.quizgroup.QuizGroupHeader

import java.io.*
import scala.util.Try

/*
 * Encapsulate platform-specific code.
 */
trait PlatformIO extends AppDependencyAccess {
  def readFile(file: File): Option[String] = readFile(file.getName)

  def readFile(fileName: String): Option[String]

  def writeToFile(fileName: String, data: String): Unit

  def readQgMetadataFromFile(qgFileName: String): Option[QuizGroupHeader]

  def readQgMetadataFromResource(qgResName: String): Option[QuizGroupHeader]

  def readResource(resName: String): Option[String]

  def findQgFileNamesFromFilesDir: Array[String]

  def findQgFileNamesFromResources: Array[String]

  def readQgMetadata(inStream: InputStream): Option[QuizGroupHeader] =
    (for
      firstLine  <- Try(readFirstLine(inStream))
      qgMetadata <- Try(Some(QuizGroupHeader(firstLine)))
    yield qgMetadata).recover { case e: Exception =>
      l.logError("Could not read quiz group file")
      None
    }.get

  // This is much faster than using Scala's Source functionality
  def readInputStream(is: InputStream): String = {
    var allText = ""

    try {
      val reader = new Array[Byte](is.available)
      while is.read(reader) != -1 do {}
      allText = allText + new String(reader)
    } catch {
      case e: IOException => l.logError("IO Exception", e.getMessage, Some(e))
    } finally {
      if is != null then {
        try {
          is.close()
        } catch {
          case e: IOException => // swallow
        }
      }
    }
    allText
  }

  def readFirstLine(is: InputStream): String =
    try {
      val br = new BufferedReader(new InputStreamReader(is))
      br.readLine
    } catch {
      case e: IOException =>
        l.logError("IO Exception", e.getMessage, Some(e))
        ""
    } finally {
      if is != null then {
        try {
          is.close()
        } catch {
          case e: IOException => // swallow
        }
      }
    }
}
