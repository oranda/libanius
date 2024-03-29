/*
 * Libanius
 * Copyright (C) 2012-2022 James McCabe <jjtmccabe@gmail.com>
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

package com.oranda.libanius.util

import com.oranda.libanius.dependencies.AppDependencyAccess

object Util extends AppDependencyAccess {

  def stopwatch[X](fn: => X, actionDescription: String): X = {
    val (result, timeTaken) = stopwatch(fn)
    l.log(s"time taken for $actionDescription was ${timeTaken}ms")
    result
  }

  def stopwatch[X](fn: => X): (X, Long) = {
    val start     = System.currentTimeMillis()
    val result    = fn
    val timeTaken = System.currentTimeMillis() - start
    (result, timeTaken)
  }
}
