/*
 * Libanius
 * Copyright (C) 2012-2016 James McCabe <james@oranda.com>
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

package com.oranda.libanius.simulation

import com.oranda.libanius.consoleui.Output._
import com.oranda.libanius.model.Quiz
import com.oranda.libanius.model.quizitem.QuizItemViewWithChoices
import com.oranda.libanius.util.{StringUtil, Util}
import scala.annotation.tailrec

import com.oranda.libanius.model.action._
import QuizItemSource._
import modelComponentsAsQuizItemSources._
import Simulation._

/*
 * Run through a whole quiz, simulating the part of the user and checking
 * for irregularities for each quiz item presented.
 */
trait Simulation {

  private var responsesProcessed: Long = 0
  private var totalMillisForUpdatingWithResponses: Long = 0
  private var totalMillisToProduceQuizItems: Long = 0
  private var totalMillisToComputeScore: Long = 0

  protected def testAllQuizItems(quiz: Quiz,
      lastQuizItem: Option[QuizItemViewWithChoices] = None) {
    val quizUpdated = testWithQuizItems(quiz, lastQuizItem)
    report(quizUpdated)
  }

  private def findQuizItem(quiz: Quiz): Option[QuizItemViewWithChoices] =
    produceQuizItem(quiz, NoParams()).orElse(findAnyUnfinishedQuizItem(quiz, NoParams()))

  @tailrec
  private def testWithQuizItems(quiz: Quiz,
      lastQuizItem: Option[QuizItemViewWithChoices] = None): Quiz = {

    if (responsesProcessed < MAX_RESPONSES) {
      val (quizItem, timeTakenToFindItem) = Util.stopwatch(findQuizItem(quiz))
      output(s"time for findQuizItem: $timeTakenToFindItem")
      if (shouldMeasureTime) totalMillisToProduceQuizItems += timeTakenToFindItem
      quizItem match {
        case Some(quizItem) =>
          lazy val allChoicesText = "\tChoices: " + quizItem.allChoices.mkString(", ")
          val strChoices = if (quizItem.useMultipleChoice) allChoicesText else ""
          output(s"$responsesProcessed. Prompt: ${quizItem.prompt}$strChoices")
          Problem.find(quiz, timeTakenToFindItem, quizItem, lastQuizItem,
              responsesProcessed) match {
            case Some(problem: Problem) =>
              problem.report()
              quiz
            case None =>
              val quizAfterResponse = respondToQuizItem(quiz, quizItem)
              testWithQuizItems(quizAfterResponse, Some(quizItem))
          }
        case _ =>
          output("No more questions found! Finished!")
          quiz
      }
    } else {
      output("Max responses reached for this simulation. Finished!")
      quiz
    }
  }

  private def respondToQuizItem(quiz: Quiz, quizItem: QuizItemViewWithChoices): Quiz = {
    val simulatedResponse = makeResponse(quizItem, quiz, responsesProcessed)
    output(s"Simulated response: $simulatedResponse")
    val quizAfterResponse = processUserResponse(quiz, simulatedResponse, quizItem)
    output(s"quizAfterResponse.numCorrectResponses: ${quizAfterResponse.numCorrectResponses}")
    responsesProcessed += 1
    quizAfterResponse
  }

  private def report(quiz: Quiz): Unit = {
    output(s"Responses processed: $responsesProcessed")

    val numResponsesMeasured = responsesProcessed - NUM_WARMUP
    def outputAverage(str: String, millis: Long) =
      output("Average millis to " + str + (millis / numResponsesMeasured))
    outputAverage("update quiz with responses: ", totalMillisForUpdatingWithResponses)
    outputAverage("find presentable items: ", totalMillisToProduceQuizItems)
    outputAverage("compute score: ", totalMillisToComputeScore)

    val score = quiz.scoreSoFar
    assert(score == 1.0, s"Score was $score")
  }

  // For now, just return the correct Answer
  protected def makeResponse(
      quizItem: QuizItemViewWithChoices,
      quiz: Quiz,
      responsesProcessed: Long): String =
    quizItem.correctResponse.value

  private def processUserResponse(
      quiz: Quiz,
      userResponseTxt: String,
      quizItem: QuizItemViewWithChoices): Quiz = {
    val correctAnswer = quizItem.correctResponse
    val isCorrect = correctAnswer.looselyMatches(userResponseTxt)
    if (isCorrect) output("Correct!") else output(s"Wrong! It's $correctAnswer")
    showScore(quiz)
    val (quizUpdated, timeTaken) = Util.stopwatch(quiz.updateWithUserResponse(
        isCorrect, quizItem.quizGroupHeader, quizItem.quizItem))

    if (shouldMeasureTime) totalMillisForUpdatingWithResponses += timeTaken
    quizUpdated
  }

  private def showScore(quiz: Quiz): Unit = {
    val (score: BigDecimal, timeTaken) = Util.stopwatch(quiz.scoreSoFar)
    if (shouldMeasureTime) totalMillisToComputeScore += timeTaken
    val formattedScore = StringUtil.formatScore(score)
    output(s"Score: $formattedScore\n")
  }

  // Don't measure processing times while the system is still "warming up"
  private def shouldMeasureTime = responsesProcessed > NUM_WARMUP
}

object Simulation {
  val MAX_RESPONSES: Int = 300000
  val NUM_WARMUP: Int = 2
}

abstract class Problem {
  def exists: Boolean
  def errorMsg: String
  def report() = output(s"PROBLEM: $errorMsg")
  def retrieve: Option[Problem] = if (exists) Some(this) else None
}

object Problem {
  def find(
      quiz: Quiz, timeTakenToFindItem: Long,
      quizItem: QuizItemViewWithChoices,
      lastQuizItem: Option[QuizItemViewWithChoices],
      responsesProcessed: Long): Option[Problem] =
    TimeTooLong(quizItem, timeTakenToFindItem, responsesProcessed).retrieve.orElse(
        QuizItemRepeated(quiz, quizItem, lastQuizItem).retrieve).orElse(
        MultipleCorrectChoices(quiz, quizItem).retrieve)
}

case class TimeTooLong(quizItem: QuizItemViewWithChoices, timeTakenToFindItem: Long,
    responsesProcessed: Long) extends Problem {
  def exists = timeTakenToFindItem > 500 && responsesProcessed > NUM_WARMUP
  def errorMsg = s"time to find a quiz item was too long for ${quizItem.prompt}"
}

case class MultipleCorrectChoices(quiz: Quiz, quizItem: QuizItemViewWithChoices) extends Problem {

  lazy val correctChoices = {
    val choices = quizItem.allChoices.toSet
    val correctResponses = quiz.findResponsesFor(quizItem.prompt.value,
        quizItem.quizGroupHeader).toSet
    choices.intersect(correctResponses)
  }

  def exists = correctChoices.size > 1

  def errorMsg =
    s"for ${quizItem.prompt} there were multiple correct choices: ${correctChoices.mkString(", ")}"
}

case class QuizItemRepeated(
    quiz: Quiz,
    quizItem: QuizItemViewWithChoices,
    lastQuizItem: Option[QuizItemViewWithChoices])
  extends Problem {

  def exists: Boolean =
    !quiz.nearTheEnd && lastQuizItem.exists(_.quizItem.samePromptAndResponse(quizItem.quizItem))

  def errorMsg = s"quiz item was repeated: ${quizItem.prompt}"
}

