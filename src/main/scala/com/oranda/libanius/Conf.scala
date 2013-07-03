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
package com.oranda.libanius

import com.typesafe.config.ConfigFactory


object Conf {
  var conf: Conf = _

  def setUp() {
    val config = ConfigFactory.load()
    conf = Conf(
        useAndroid = config.getBoolean("libanius.useAndroid"),
        numCorrectAnswersRequired = config.getInt("libanius.numCorrectAnswersRequired"),
        fileVocab = config.getString("libanius.file.vocab"),
        fileQuizRootInit = config.getString("libanius.file.quizRoot"),
        fileDictionary = config.getString("libanius.file.dictionary"),
        resQuizPublic = config.getString("libanius.res.quizPublic"),
        resDictPublic = config.getString("libanius.res.dictPublic"),
        // Variables passed between activities
        keyWord = config.getString("libanius.interActivity.keyWord"),
        value = config.getString("libanius.interActivity.value")
    )
  }

  // Mock configuration for tests
  def setUpDummy() {
    conf = Conf(
      useAndroid = false,
      numCorrectAnswersRequired = 4,
      fileVocab = "",
      fileQuizRootInit = "",
      fileDictionary = "",
      resQuizPublic = "",
      resDictPublic = "",
      // Variables passed between activities
      keyWord = "",
      value = ""
    )
  }

}


case class Conf(useAndroid: Boolean, numCorrectAnswersRequired: Int,
    fileVocab: String, fileQuizRootInit: String, fileDictionary: String,
    resQuizPublic: String, resDictPublic: String, keyWord: String, value: String) {

  lazy val fileQuiz = Conf.conf.fileQuizRoot + ".qui"
  lazy val fileQuizLastBackup = Conf.conf.fileQuizRoot + "Backup" + ".qui"

  // things that can change
  var fileQuizRoot = fileQuizRootInit
}
