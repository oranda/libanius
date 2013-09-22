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

package com.oranda.libanius.model.quizitem

case class TextValue(override val value: String) extends Value[String](value) {

  override def toString = value
  def matches(otherText: String) = value == otherText

  override def hasSameStart(otherValue: String): Int => Boolean =
    TextValue.hasSameStart(value, otherValue)
  override def hasSameEnd(otherValue: String): Int => Boolean =
    TextValue.hasSameEnd(value, otherValue)
}

object TextValue {
  def hasSameStart(value: String, otherValue: String): Int => Boolean =
    (numOfLetters: Int) => otherValue != value &&
        value.take(numOfLetters) == otherValue.take(numOfLetters)

  def hasSameEnd(value: String, otherValue: String): Int => Boolean =
    (numOfLetters: Int) => otherValue != value &&
        value.takeRight(numOfLetters) == otherValue.takeRight(numOfLetters)
}
