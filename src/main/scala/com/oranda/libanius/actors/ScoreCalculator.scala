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
package com.oranda.libanius.actors

import com.oranda.libanius.util.{Util, Platform}
import akka.actor.Actor
import com.oranda.libanius.model.wordmapping.QuizOfWordMappings

class ScoreCalculator extends Actor {
  def receive = {
    case Calculate(quiz: QuizOfWordMappings) ⇒
      Util.log("Received Calculate message")
      sender ! Result((Util.stopwatch(quiz.scoreSoFar, "scoreSoFar") * 100).toString)
  }
}
