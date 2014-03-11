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

package com.oranda.libanius.model

/*
 * A MemoryLevel helps determine whether quiz items are "presentable". It specifies a minimum
 * interval before a quiz item can be presented again, depending on the number of consecutive
 * correct responses there have already been for that quiz item.
 * Look up the concept of "spaced repetition".
 *
 * numCorrectResponses: how many times this item should have been answered correctly
 * diffInPromptNumMinimum: how long ago it was last answered 
 */
case class MemoryLevel(numCorrectResponses: Int, diffInPromptNumMinimum: Int) {

  def isPresentable(currentPromptNum : Int, promptNumInMostRecentResponse: Option[Int],
      quizItemNumCorrectResponses: Int): Boolean = {
    def wasNotTooRecentlyUsed = promptNumInMostRecentResponse.forall {
      case promptNumInMostRecentResponse =>
        val diffInPromptNum = currentPromptNum - promptNumInMostRecentResponse
        diffInPromptNum >= diffInPromptNumMinimum
    }
    (quizItemNumCorrectResponses == numCorrectResponses) && wasNotTooRecentlyUsed
  }
}
