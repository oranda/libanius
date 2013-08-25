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
import com.oranda.libanius.model.wordmapping.QuizGroupHeader
import com.oranda.libanius.dependencies.{AppDependencies, Conf}

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

  override def readWmgMetadataFromFile(wmgFileName: String): Option[QuizGroupHeader] =
    readWmgMetadata(conf.filesDir + wmgFileName)

  override def readWmgMetadataFromResource(wmgResName: String): Option[QuizGroupHeader] =
    readWmgMetadata(conf.resourcesDir + wmgResName)

  private def readWmgMetadata(wmgPath: String): Option[QuizGroupHeader] = {
    val file = new File(wmgPath)
    if (file.exists) {
      l.log("readWmgMetadata: reading file " + wmgPath)
      readWmgMetadata(new FileInputStream(file))
    } else {
      l.logError("File not found: " + wmgPath)
      None
    }
  }

  override def findWmgFileNamesFromFilesDir =
    new File(conf.filesDir).listFiles.filter(_.getName.endsWith(".wmg")).map(_.getName)

  override def findWmgFileNamesFromResources =
    new File(conf.resourcesDir).listFiles.filter(_.getName.startsWith("wmg")).map(_.getName)
}