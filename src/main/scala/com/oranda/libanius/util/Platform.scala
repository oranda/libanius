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

import com.oranda.libanius.Conf
import android.content.Context
import com.oranda.libanius.io.{StandardIO, AndroidIO}

/*
 * Encapsulate platform-specific code.
 */
trait Platform {

  /**
   * The String processing needs to be very fast, especially on a limited Android device.
   * The Android splitter utilities are faster than String.split()
   *
   * The gnarly ThreadLocal code should go away on the switch to Akka
   */
  def getSplitter(char: java.lang.Character): StringSplitter = {
    if (Conf.conf.useAndroid)
      new StringSplitterAndroid(char)
    else
      new StringSplitterDefault(char)
  }

  def logError(message: String, t: Throwable) {
    logError(message, "Libanius", Some(t))
  }

  def logError(message: String, module: String = "Libanius", t: Option[Throwable] = None) {
    log("ERROR: " + message, module, t)
  }

  def log(message: String, module: String = "Libanius", t: Option[Throwable] = None) {
    if (Conf.conf.enableLogging) {
      if (Conf.conf.useAndroid)
        AndroidIO.log(message, module, t)
      else
        StandardIO.log(module, message, t)
    }
  }

  def writeToFile(fileName: String, data: String, ctx: Option[Context] = None) =
    if (Conf.conf.useAndroid)
      AndroidIO.writeToFile(fileName, data, ctx.get)
    else
      StandardIO.writeToFile(fileName, data)
}