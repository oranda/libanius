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

import com.oranda.libanius.io.{DefaultIO, PlatformIO}
import com.oranda.libanius.util.{StringSplitterFactoryDefault, StringSplitterFactory}

/*
 * Defaults for application dependencies. Can be used in a PC/Unix environment.
 */
object AppDependenciesDefault extends AppDependencies {

  lazy val l: Logger            = new LoggerDefault
  lazy val c: ConfigProvider    = new ConfigProviderDefault
  lazy val dataStore: DataStore = new DataStoreDefault
  lazy val stringSplitterFactory: StringSplitterFactory = new StringSplitterFactoryDefault
}
