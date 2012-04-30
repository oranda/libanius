/*
 * Copyright 2012 James McCabe <jamesc@oranda.com>
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

import _root_.java.io.FileOutputStream
import _root_.java.io.OutputStreamWriter
import _root_.java.io.FileInputStream
import _root_.java.io.InputStreamReader
import _root_.java.io.BufferedReader
import _root_.java.io.IOException
import java.io.File
import java.io.FileWriter
import com.oranda.libanius.model.Quiz
import scala.io.BufferedSource
import scala.io.Source


object StandardIO {

  def readFile(fileName: String): String = {
    val myFile = new File(fileName)
    val src = Source.fromFile(myFile)
    src.mkString
  }    
  
  def save(fileName: String, strToSave: String) {
	val file = new File(fileName);
	writeToFile(fileName, strToSave)
  }
  
  def writeToFile(fileName: String, data: String) = 
    using (new FileWriter(fileName)) {
      fileWriter => fileWriter.write(data)
  }
  
  def using[A <: {def close(): Unit}, B](param: A)(f: A => B): B =
	  try { f(param) } finally { param.close() }   
  
}