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

  def companion[T](name : String)(implicit man: Manifest[T]) : Try[T] =
    Try(Class.forName(name + "$").getField("MODULE$").get(man.runtimeClass).asInstanceOf[T])

  val fqcnDependenciesOverride = "com.oranda.libanius.dependencies.AppDependenciesOverride"
  lazy val appDependenciesObject: AppDependencies =
    companion[AppDependencies](fqcnDependenciesOverride).getOrElse(AppDependenciesDefault)

  lazy val l = appDependenciesObject.l
  lazy val conf = appDependenciesObject.conf
  lazy val io = appDependenciesObject.io
  lazy val dataStore = appDependenciesObject.dataStore
  lazy val stringSplitterFactory = appDependenciesObject.stringSplitterFactory
}
