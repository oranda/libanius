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

package com.oranda.libanius.dependencies

trait Logger {

  def logError(message: String, t: Throwable) {
    logError(message, "Libanius", Some(t))
  }

  def logError(message: String, module: String = "Libanius", t: Option[Throwable] = None) {
    log("ERROR: " + message, module, t)
  }

  def log(message: String, module: String = "Libanius", t: Option[Throwable] = None) {
    if (AppDependencies.conf.enableLogging)
      logImpl(message, module, t)
  }

  def logImpl(message: String, module: String = "Libanius", t: Option[Throwable] = None)
}

class LoggerDefault extends Logger {
  override def logImpl(message: String, module: String = "Libanius", t: Option[Throwable] = None) {
    println(module + ": " + message)
    t.foreach(_.printStackTrace())
  }
}

