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

import com.typesafe.config.ConfigFactory

abstract class ConfigProvider {
  def conf: Conf
}

/*
 * Configuration in a PC context.
 */
class ConfigProviderDefault extends ConfigProvider {
  lazy val config = ConfigFactory.load()
  override def conf =
    new Conf(
      enableLogging = config.getBoolean("libanius.enableLogging"),
      numCorrectAnswersRequired = config.getInt("libanius.numCorrectAnswersRequired"),
      fileQuizRoot = config.getString("libanius.file.quizRoot"),
      filesDir = config.getString("libanius.file.filesDir"),
      resourcesDir = config.getString("libanius.file.resourcesDir"),
      resQuizPublic = config.getString("libanius.res.quizPublic")
    )
}

/*
 * Mock configuration for tests.
 */
class ConfigProviderMock extends ConfigProvider {
  override def conf =
    new Conf(
      enableLogging = false,
      numCorrectAnswersRequired = 4,
      fileQuizRoot = "",
      filesDir = "",
      resourcesDir = "",
      resQuizPublic = ""
    )
}
