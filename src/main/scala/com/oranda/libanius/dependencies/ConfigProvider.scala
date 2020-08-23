/*
 * Libanius
 * Copyright (C) 2012-2020 James McCabe <james@oranda.com>
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
      email = config.getString("libanius.email"),
      defaultPromptType = config.getString("libanius.defaultPromptType"),
      defaultResponseType = config.getString("libanius.defaultResponseType"),
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
      email = "",
      defaultPromptType = "",
      defaultResponseType = "",
      fileQuizRoot = "",
      filesDir = "",
      resourcesDir = "",
      resQuizPublic = ""
    )
}
