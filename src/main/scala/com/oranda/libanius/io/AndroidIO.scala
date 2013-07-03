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

import com.oranda.libanius.Conf
import _root_.android.content.Context
import _root_.java.io._
import _root_.android.util.Log
import scala.io.Source

object AndroidIO {

  def readFile(ctx: Context, fileName: String): String = {
    def fileToInputStream(ctx: Context) = ctx.openFileInput(fileName)
    readInputStream(ctx, fileToInputStream)
  }
  
  def readResource(ctx: Context, resName: String): String = {
    def resID = ctx.getResources().getIdentifier(resName, "raw", ctx.getPackageName)
    def resourceToInputStream(ctx: Context) = ctx.getResources().openRawResource(resID)
    readInputStream(ctx, resourceToInputStream)
  }
  
  // This is much faster than using Scala's Source functionality
  def readInputStream(ctx: Context, inStreamGetter: Context => InputStream): String = {
    
    var allText = ""    
    var is: InputStream = null
    
    try { 
      val is = inStreamGetter(ctx)
      val reader = new Array[Byte](is.available)
      while (is.read(reader) != -1) {}
        allText = allText + new String(reader)
    } catch {
      case e: IOException =>
        Log.e("IO Exception", e.getMessage, e)
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
          
  
  def save(ctx: Context, fileName: String, fileNameBackup: String, strToSave: String) {
	val file = new File(fileName)
	val file2 = new File(fileNameBackup)
	file2.delete()
	//Platform.log("AndroidIO.save", "Renaming " + fileName + " to " + fileNameBackup)
	file.renameTo(file2) // Doesn't seem to work, but not crucial

	writeToFile(Conf.conf.fileQuiz, strToSave, ctx)
  }

  def writeToFile(fileName:String, data:String, ctx:Context) = {
    val fOut : FileOutputStream = ctx.openFileOutput(fileName, Context.MODE_PRIVATE);
    fOut.write(data.getBytes())  
    fOut.close()
  }
  
}