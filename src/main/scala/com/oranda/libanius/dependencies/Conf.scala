/*
 * Libanius
 * Copyright (C) 2012-2014 James McCabe <james@oranda.com>
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
      numCorrectAnswersRequired = config.getInt("libanius.numCorrectAnswersRequired"),
      useMultipleChoiceUntil = config.getInt("libanius.useMultipleChoiceUntil"),
      email = config.getString("libanius.email"),
      fileQuizRoot = config.getString("libanius.file.quizRoot"),
      filesDir = config.getString("libanius.file.filesDir"),
      resourcesDir = config.getString("libanius.file.resourcesDir"),
      resQuizPublic = config.getString("libanius.res.quizPublic")
    )
  }

  // Mock configuration for tests
  def setUpForTest() = {
    new Conf(
      enableLogging = false,
      numCorrectAnswersRequired = 4,
      useMultipleChoiceUntil = 4,
      email = "",
      fileQuizRoot = "",
      filesDir = "",
      resourcesDir = "",
      resQuizPublic = ""
    )
  }

  // Configuration for preparing data files before the app is deployed
  def setUpForParsing(fileQuizRoot: String) = {
    new Conf(
      enableLogging = true,
      numCorrectAnswersRequired = 4,
      useMultipleChoiceUntil = 4,
      email = "",
      fileQuizRoot,
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
    val numCorrectAnswersRequired: Int,
    val useMultipleChoiceUntil: Int,
    val email: String,
    val fileQuizRoot: String,
    val filesDir: String,
    val resourcesDir: String,
    val resQuizPublic: String) {

  lazy val fileQuiz = fileQuizRoot + ".qui"
  lazy val fileQuizLastBackup = fileQuizRoot + "Backup" + ".qui"
}

