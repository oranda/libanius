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

import com.oranda.libanius.io.StandardIO
import com.oranda.libanius.util.{StringSplitterFactoryDefault, StringSplitterFactory}

/*
 * This is the central location for dependencies like logging that change with the
 * context of the application (Android vs Desktop vs Webapp vs Test, etc).
 *
 * The Cake Pattern is not appropriate here, because runtime DI is needed: the
 * components may be overridden from different jar's.
 */
object AppDependencies {
  var conf = Conf.setUp()
  var logger: Logger = new LoggerDefault
  var dataStore = new DataStore(new StandardIO)
  var stringSplitterFactory: StringSplitterFactory = new StringSplitterFactoryDefault

  def init(_conf: Conf, _logger: Logger, _dataStore: DataStore,
      _stringSplitterFactory: StringSplitterFactory) {
    conf = _conf
    logger = _logger
    dataStore = _dataStore
    stringSplitterFactory = _stringSplitterFactory
  }
}
