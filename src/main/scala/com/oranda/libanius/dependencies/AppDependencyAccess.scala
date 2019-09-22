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

import scala.util.Try

/*
 * This is the central location for dependencies like logging that change with the
 * context of the application (Android vs Desktop vs Webapp vs Test, etc). Defaults
 * are provided for the ConsoleUI.
 *
 * Runtime dependency injection is done using a custom scheme that relies (minimally)
 * on reflection. If libanius is used as a library by another app, the default app
 * dependencies can be overridden by creating a class that extends the trait AppDependencies
 * and has the path:
 *
 * com.oranda.libanius.dependencies.AppDependenciesOverride
 */
trait AppDependencyAccess {

  def companion[T](name : String)(implicit man: Manifest[T]): Try[T] =
    Try(Class.forName(name + "$").getField("MODULE$").get(man.runtimeClass).asInstanceOf[T])

  val fqcnDependenciesOverride = "com.oranda.libanius.dependencies.AppDependenciesOverride"
  lazy val appDependenciesObject: AppDependencies =
    companion[AppDependencies](fqcnDependenciesOverride).getOrElse(AppDependenciesDefault)

  lazy val l = appDependenciesObject.l
  lazy val conf = appDependenciesObject.conf
  lazy val dataStore = appDependenciesObject.dataStore
  lazy val stringSplitterFactory = appDependenciesObject.stringSplitterFactory
}
