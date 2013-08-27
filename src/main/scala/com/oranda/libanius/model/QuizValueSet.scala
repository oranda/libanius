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

package com.oranda.libanius.model

import com.oranda.libanius.dependencies.AppDependencies
import com.oranda.libanius.util.StringUtil
import scala.util.Random

abstract class QuizValueSet(val values: List[QuizValueWithUserAnswers] = Nil) {

  type MyType <: QuizValueSet

  val l = AppDependencies.logger

  def updated(values: List[QuizValueWithUserAnswers]): MyType

  override def toString = values.toString

  // Example: contract:696,697;698/treaty:796;798
  def toCustomFormat(strBuilder: StringBuilder) =
    StringUtil.mkString(strBuilder, values, wmvToCustomFormat, '/')

  def wmvToCustomFormat(strBuilder: StringBuilder, value: QuizValueWithUserAnswers):
      StringBuilder =
    value.toCustomFormat(strBuilder)

  def strings: Iterable[String] = values.map(_.toString )

  def size = values.size

  def numItemsAndCorrectAnswers: Pair[Int, Int] = Pair(size, numCorrectAnswers)

  def numCorrectAnswers = {
    /*
     * This functional version is about twice as slow as the version actually used:
     *
     * values.iterator.foldLeft(0)(_ + _.numCorrectAnswersInARow)
     */
    var numCorrectAnswers = 0
    values.foreach { wmv => numCorrectAnswers += wmv.numCorrectAnswersInARow }
    numCorrectAnswers
  }


  def replaceWmv(valueNew: QuizValueWithUserAnswers): MyType = {
    val quizValueSet: MyType = filterOut(valueNew.value)
    quizValueSet.addValueToFront(valueNew).asInstanceOf[MyType]
  }

  def addValueToFront(quizValue: QuizValueWithUserAnswers): MyType = {
    val newValues =
      if (!values.contains(quizValue)) quizValue +: values
      else values
    updated(newValues)
  }

  def addValueToEnd(quizValue: QuizValueWithUserAnswers): MyType

  def filterOut(value: String): MyType =
    updated(values.filterNot(_.value == value))

  def findPresentableWordMappingValue(currentPromptNumber: Int): Option[QuizValueWithUserAnswers] =
    values.iterator.find(_.isPresentable(currentPromptNumber))

  def findAnyUnfinishedWordMappingValue: Option[QuizValueWithUserAnswers] =
    values.iterator.find(_.isUnfinished)

  def findRandomWordValue(): String = {
    val randomIndex = Random.nextInt(values.size)
    val valueArray: Array[QuizValueWithUserAnswers] = values.toArray[QuizValueWithUserAnswers]
    valueArray(randomIndex).value
  }

  def findValue(value: String): Option[QuizValueWithUserAnswers] = values.find(_.value == value)

  def containsValue(value: String): Boolean = findValue(value).isDefined

  def valueBeginningWith(valueStart: String) = values.find(_.value.startsWith(valueStart))
}
