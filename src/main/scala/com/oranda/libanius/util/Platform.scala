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

package com.oranda.libanius.util

import scala.collection.JavaConverters._
import java.util.HashMap
import com.oranda.libanius.Props
import java.util.Iterator
import android.text.TextUtils
import android.util.Log

/*
 * Encapsulate platform-specific code.
 */
trait Platform {
 
  def getSplitter(char: java.lang.Character): StringSplitter =
    if (Props.ANDROID)
      new StringSplitterAndroid(char)
    else 
      new StringSplitterDefault(char)
   
  def log(module: String, message: String) =
    if (Props.ANDROID)
      Log.d(module, message)
    else
      System.out.println(module + ": " + message)
      
  def log(module: String, message: String, t: Throwable) =
    if (Props.ANDROID)
      Log.d(module, message, t)
    else {
      System.out.println(module + ": " + message)
      t.printStackTrace()
    }  
}