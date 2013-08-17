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

import akka.actor.{ActorRef, Props, Actor}
import akka.routing.RoundRobinRouter
import com.oranda.libanius.model.wordmapping.{QuizOfWordMappings}
import com.oranda.libanius.util.Util


class QuizSaveWorker(quiz: QuizOfWordMappings, listener: ActorRef) extends Actor {

  var nrOfResults: Int = _
  val nrOfGroups = quiz.wordMappingGroups.size

  val workerRouter = context.actorOf(Props(new QuizGroupSaveWorker).withRouter(
      RoundRobinRouter(nrOfGroups)), name = "quizGroupRouter")

  def receive = {
    case SaveQuiz(ctx) =>
      Util.log("QuizSaveWorker: got SaveQuiz message, number of groups: " + nrOfGroups)
      for (wmg <- quiz.wordMappingGroups) {
        Util.log("Sending SaveQuizGroup message for quizGroup " + wmg.header)
        workerRouter ! SaveQuizGroup(wmg, ctx)
      }
    case Done =>
      Util.log("Received Done message from a quiz group worker")
      nrOfResults += 1
      Util.log("nrOfResults: " + nrOfResults)
      if (nrOfResults == nrOfGroups) {
        Util.log("QuizSaveWorker: sending Done message to listener")
        listener ! Done
      }
  }
}