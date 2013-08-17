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

import com.oranda.libanius.model.wordmapping.{WordMappingGroup, QuizOfWordMappings}
import android.content.Context

sealed trait ActorMessage
object ActorMessage

case class Calculate(quiz: QuizOfWordMappings) extends ActorMessage
case class Result(value: String) extends ActorMessage

case class SaveQuiz(ctx: Option[Context]) extends ActorMessage
case class SaveQuizGroup(quizGroup: WordMappingGroup, ctx: Option[Context]) extends ActorMessage
case class Done extends ActorMessage
