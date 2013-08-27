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

package com.oranda.libanius.io

import java.io._
import scala.util.Try
import com.oranda.libanius.dependencies.AppDependencies
import com.oranda.libanius.model.QuizGroupHeader

/*
 * Encapsulate platform-specific code.
 */
trait PlatformIO {

  val l = AppDependencies.logger

  def readFile(file: File): Option[String] = readFile(file.getName)

  def readFile(fileName: String): Option[String]

  def writeToFile(fileName: String, data: String)

  def readQgMetadataFromFile(qgFileName: String): Option[QuizGroupHeader]

  def readQgMetadataFromResource(qgResName: String): Option[QuizGroupHeader]

  def readResource(resName: String): Option[String]

  def findQgFileNamesFromFilesDir: Array[String]

  def findQgFileNamesFromResources: Array[String]

  def readQgMetadata(inStream: InputStream): Option[QuizGroupHeader] = {
    var firstLine = ""
    (for {
      firstLine <- Try(readFirstLine(inStream))
      qgMetadata <- Try(Some(QuizGroupHeader(firstLine)))
    } yield qgMetadata).recover {
      case e: Exception =>
        l.logError("Could not read qg file, firstLine " + firstLine, e)
        None
    }.get
  }

  // This is much faster than using Scala's Source functionality
  def readInputStream(is: InputStream): String = {
    var allText = ""

    try {
      val reader = new Array[Byte](is.available)
      while (is.read(reader) != -1) {}
      allText = allText + new String(reader)
    } catch {
      case e: IOException => l.logError("IO Exception", e.getMessage, Some(e))
    } finally {
      if (is != null) {
        try {
          is.close()
        } catch {
          case e: IOException => // swallow
        }
      }
    }
    allText
  }

  // TODO: combine with readInputStream
  def readFirstLine(is: InputStream): String = {
    try {
      val br = new BufferedReader(new InputStreamReader(is))
      br.readLine
    } catch {
      case e: IOException =>
        l.logError("IO Exception", e.getMessage, Some(e))
        ""
    } finally {
      if (is != null) {
        try {
          is.close()
        } catch {
          case e: IOException => // swallow
        }
      }
    }
  }
}
