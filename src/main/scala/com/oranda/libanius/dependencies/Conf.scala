/*
 * Libanius
 * Copyright (C) 2012-2020 James McCabe <jjtmccabe@gmail.com>
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

object Conf {

  def setUp() = {
    val config = ConfigFactory.load()
    new Conf(
      enableLogging = config.getBoolean("libanius.enableLogging"),
      email = config.getString("libanius.email"),
      defaultPromptType = config.getString("defaultPromptType"),
      defaultResponseType = config.getString("defaultResponseType"),
      filesDir = config.getString("libanius.file.filesDir"),
      resourcesDir = config.getString("libanius.file.resourcesDir"),
      resQuizPublic = config.getString("libanius.res.quizPublic")
    )
  }

  // Mock configuration for tests
  def setUpForTest() = {
    new Conf(
      enableLogging = false,
      email = "",
      defaultPromptType = "",
      defaultResponseType = "",
      filesDir = "",
      resourcesDir = "",
      resQuizPublic = ""
    )
  }

  // Configuration for preparing data files before the app is deployed
  def setUpForParsing = {
    new Conf(
      enableLogging = true,
      email = "",
      defaultPromptType = "",
      defaultResponseType = "",
      filesDir = "",
      resourcesDir = "",
      resQuizPublic = ""
    )
  }
}

/*
 * This could be extended in applications that use these classes as a library.
 */
class Conf(
    val enableLogging: Boolean,
    val email: String,
    val defaultPromptType: String,
    val defaultResponseType: String,
    val filesDir: String,
    val resourcesDir: String,
    val resQuizPublic: String)

