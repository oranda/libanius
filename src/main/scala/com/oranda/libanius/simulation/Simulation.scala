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

package com.oranda.libanius.simulation

import com.oranda.libanius.consoleui.Output._
import com.oranda.libanius.model.Quiz
import com.oranda.libanius.model.quizitem.QuizItemViewWithChoices
import com.oranda.libanius.util.{StringUtil, Util}
import com.oranda.libanius.model.quizgroup.QuizGroupWithHeader
import scala.annotation.tailrec

/*
 * Run through a whole quiz, simulating the part of the user and checking
 * for irregularities for each quiz item presented.
 */
trait Simulation {

  private var responsesProcessed: Long = 0
  private var totalMillisForUpdatingWithResponses: Long = 0
  private var totalMillisToFindPresentableItems: Long = 0
  private var totalMillisToComputeScore: Long = 0

  protected def testAllQuizItems(quiz: Quiz,
      lastQuizItem: Option[QuizItemViewWithChoices] = None) {
    val maxResponses = 300000
    testWithQuizItems(quiz, lastQuizItem)(maxResponses)
    report(quiz)
  }

  @tailrec
  private def testWithQuizItems(quiz: Quiz,
      lastQuizItem: Option[QuizItemViewWithChoices] = None) (implicit maxResponses: Long) {

    if (responsesProcessed < maxResponses) {

      val (presentableQuizItem, timeTakenToFindItem) = Util.stopwatch(quiz.findPresentableQuizItem)
      output("time for findPresentableQuizItem: " + timeTakenToFindItem)
      if (shouldMeasureTime) totalMillisToFindPresentableItems += timeTakenToFindItem

      presentableQuizItem match {
        case (Some((quizItem, qgWithHeader))) =>
          val strChoices =
            if (quizItem.useMultipleChoice)
              "\tChoices: " + quizItem.allChoices.mkString(", ")
            else ""
          output(responsesProcessed + ". Prompt: " + quizItem.prompt + strChoices)
          val problem = findProblem(timeTakenToFindItem, quizItem, lastQuizItem, quiz)
          problem.foreach(output(_))
          if (!problem.isDefined) {
            val simulatedResponse = makeResponse(quizItem, quiz, responsesProcessed)
            output("Simulated response: " + simulatedResponse)
            val quizAfterResponse = processUserResponse(quiz, simulatedResponse, quizItem)

            responsesProcessed += 1
            testWithQuizItems(quizAfterResponse, Some(quizItem))
          }
        case _ =>
          output("No more questions found! Finished!")
      }
    } else {
      output("Max responses reached for this simulation. Finished!")
    }
  }

  private def findProblem(timeTakenToFindItem: Long,
      quizItem: QuizItemViewWithChoices, lastQuizItem: Option[QuizItemViewWithChoices],
      quiz: Quiz): Option[String] =
    if (timeTakenToFindItem > 500 && responsesProcessed > 3)
      Some("time taken to find a presentable quiz item was too long for " + quizItem.prompt)
    else if (quizItemWasRepeated(quizItem, lastQuizItem, quiz))
      Some("quiz item was repeated")
    else multipleCorrectChoices(quizItem, quiz) // may return None

  private def quizItemWasRepeated(quizItem: QuizItemViewWithChoices,
      lastQuizItem: Option[QuizItemViewWithChoices], quiz: Quiz): Boolean =
    !quiz.nearTheEnd && lastQuizItem.exists(
        _.quizItem.samePromptAndResponse(quizItem.quizItem))


  /*
   * If there are multiple correct responses in the quiz item choices, return them
   * as a String, otherwise None.
   */
  private def multipleCorrectChoices(quizItem: QuizItemViewWithChoices, quiz: Quiz):
      Option[String] = {
    val choices = quizItem.allChoices.toSet
    val correctResponses = quiz.findResponsesFor(quizItem.prompt.value,
        quizItem.quizGroupHeader).toSet
    val correctChoices = choices.intersect(correctResponses)

    if (correctChoices.size == 1) None // no problem
    else Some("for " + quizItem.prompt + " there were multiple correct choices: " +
        correctChoices.mkString(", "))
  }


  private def report(quiz: Quiz) {
    output("Responses processed: " + responsesProcessed)

    val numResponsesMeasured = responsesProcessed - 2
    output("Average millis for updating quiz with responses: " +
        (totalMillisForUpdatingWithResponses / numResponsesMeasured))
    output("Average millis to find presentable items: " +
        (totalMillisToFindPresentableItems / numResponsesMeasured))
    output("Average millis to compute score: " +
        (totalMillisToComputeScore / numResponsesMeasured))
  }

  // For now, just return the correct Answer
  protected def makeResponse(quizItem: QuizItemViewWithChoices, quiz: Quiz,
      responsesProcessed: Long): String =
    quizItem.correctResponse.value

  private def processUserResponse(quiz: Quiz, userResponseTxt: String,
      quizItem: QuizItemViewWithChoices): Quiz = {
    val correctAnswer = quizItem.correctResponse
    val isCorrect = correctAnswer.looselyMatches(userResponseTxt)
    if (isCorrect) output("Correct!") else output("Wrong! It's " + correctAnswer)
    showScore(quiz)
    val (quizUpdated, timeTaken) = Util.stopwatch(quiz.updateWithUserResponse(
        isCorrect, quizItem.quizGroupHeader, quizItem.quizItem))

    if (shouldMeasureTime) totalMillisForUpdatingWithResponses += timeTaken
    quizUpdated
  }

  private def showScore(quiz: Quiz) {
    val (score: BigDecimal, timeTaken) = Util.stopwatch(quiz.scoreSoFar)
    if (shouldMeasureTime) totalMillisToComputeScore += timeTaken
    val formattedScore = StringUtil.formatScore(score)
    output("Score: " + formattedScore + "\n")
  }

  // Don't measure processing times while the system is still "warming up"
  private def shouldMeasureTime = responsesProcessed > 2
}
