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

import java.io.File
import java.io.FileWriter
import scala.io.Source


object StandardIO {

  def readFile(fileName: String): String = {
    val myFile = new File(fileName)
    val src = Source.fromFile(myFile)
    src.mkString
  }    
  
  def save(fileName: String, strToSave: String) {
    val file = new File(fileName)
    writeToFile(fileName, strToSave)
  }
  
  def writeToFile(fileName: String, data: String) = 
    using (new FileWriter(fileName)) (_.write(data))
  
  private def using[A <: {def close(): Unit}, B](param: A)(f: A => B): B =
	  try { f(param) } finally { param.close() }

  def log(message: String, module: String = "QuizScreen", t: Option[Throwable] = None) {
    System.out.println(module + ": " + message)
    t.foreach(_.printStackTrace())
  }
  
}