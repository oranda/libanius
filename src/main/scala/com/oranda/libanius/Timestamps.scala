/*
 * Copyright 2012 James McCabe <james@oranda.com>
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

trait Timestamps {

  private[this] var timestampsLastCorrectAnswers = List[Long]()
  
  def updateTimestamps(thereJustOccurredACorrectAnswer: Boolean) {
    if (thereJustOccurredACorrectAnswer) {
      val currentTime = System.currentTimeMillis
      timestampsLastCorrectAnswers ::= currentTime
      /* 
       * Purge timestamps older than one minute. This leaves the length of the 
       * list as a measure of the number of correct answers per minute.
       */
      timestampsLastCorrectAnswers = timestampsLastCorrectAnswers.filter(
          _ > currentTime - 60000)
    } 
  }
  
  def answerSpeed = timestampsLastCorrectAnswers.size
}