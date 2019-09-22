/*
 * Libanius
 * Copyright (C) 2012-2019 James McCabe <james@oranda.com>
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

package com.oranda.libanius.dependencies

abstract class Logger extends AppDependencyAccess {

  def logError(message: String, t: Throwable): Unit =
    logError(message, "Libanius", Option(t))

  def logError(message: String, module: String = "Libanius", t: Option[Throwable] = None): Unit =
    log(s"ERROR: $message", module, t)

  def log(message: String, module: String = "Libanius", t: Option[Throwable] = None): Unit =
    if (conf.enableLogging)
      logImpl(message, module, t)

  def logImpl(message: String, module: String = "Libanius", t: Option[Throwable] = None)
}

class LoggerDefault extends Logger {
  override def logImpl(
      message: String,
      module: String = "Libanius",
      t: Option[Throwable] = None): Unit = {
    println(s"$module: $message")
    t.foreach(_.printStackTrace())
  }
}

