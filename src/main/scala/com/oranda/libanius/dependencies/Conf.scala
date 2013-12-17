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

object Conf {

  def setUp() = {
    val config = ConfigFactory.load()
    new Conf(
      enableLogging = config.getBoolean("libanius.enableLogging"),
      numCorrectAnswersRequired = config.getInt("libanius.numCorrectAnswersRequired"),
      useMultipleChoiceUntil = config.getInt("libanius.useMultipleChoiceUntil"),
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
    val fileQuizRoot: String,
    val filesDir: String,
    val resourcesDir: String,
    val resQuizPublic: String) {

  lazy val fileQuiz = fileQuizRoot + ".qui"
  lazy val fileQuizLastBackup = fileQuizRoot + "Backup" + ".qui"
}

