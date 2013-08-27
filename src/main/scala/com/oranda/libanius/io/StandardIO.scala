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
import scala.io.Source
import com.oranda.libanius.dependencies.AppDependencies
import com.oranda.libanius.model.QuizGroupHeader

class StandardIO extends PlatformIO {

  private[this] lazy val conf = AppDependencies.conf

  def readFile(fileName: String): Option[String] = {
    l.log("Reading file " + fileName)
    val myFile = new File(fileName)
    val src = Source.fromFile(myFile)
    Some(src.mkString)
  }    
  
  def save(fileName: String, strToSave: String) {
    writeToFile(fileName, strToSave)
  }
  
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
      l.log("readQgMetadata: reading file " + qgPath)
      readQgMetadata(new FileInputStream(file))
    } else {
      l.logError("File not found: " + qgPath)
      None
    }
  }

  override def findQgFileNamesFromFilesDir =
    new File(conf.filesDir).listFiles.filter(_.getName.endsWith(".qg")).map(_.getName)

  override def findQgFileNamesFromResources =
    new File(conf.resourcesDir).listFiles.filter(_.getName.startsWith("qg")).map(_.getName)
}