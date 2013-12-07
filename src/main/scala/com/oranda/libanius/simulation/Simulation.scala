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

package com.oranda.libanius.simulation

import com.oranda.libanius.consoleui.Output._
import com.oranda.libanius.model.Quiz
import com.oranda.libanius.model.quizitem.QuizItemViewWithChoices
import com.oranda.libanius.util.{StringUtil, Util}
import com.oranda.libanius.model.quizgroup.QuizGroupWithHeader

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
    val maxResponses = 1000
    testWithQuizItems(quiz, lastQuizItem)(maxResponses)
    report(quiz)
  }

  private def testWithQuizItems(quiz: Quiz,
      lastQuizItem: Option[QuizItemViewWithChoices] = None) (implicit maxResponses: Long) {

    doGarbageCollection()

    if (responsesProcessed < maxResponses) {

      val (presentableQuizItem, timeTakenToFindItem) = Util.stopwatch(quiz.findPresentableQuizItem)
      output("time for findPresentableQuizItem: " + timeTakenToFindItem)
      if (shouldMeasureTime) totalMillisToFindPresentableItems += timeTakenToFindItem

      presentableQuizItem match {
        case (Some((quizItem, qgWithHeader))) =>
          // This will recursively call testWithQuizItems
          processQuizItem(timeTakenToFindItem, quizItem, lastQuizItem, quiz, qgWithHeader)
        case _ =>
          output("No more questions found! Finished!")
      }
    } else {
      output("Max responses reached for this simulation. Finished!")
    }
  }

  /**
   * In a fast simulation, garbage collection becomes a problem and may skew
   * the performance statistics.
   * (In normal user operation there are pauses so it's not a problem.)
   */
  private def doGarbageCollection() {
    if (responsesProcessed % 10 == 0) {
      output("Deliberate garbage collection pause")
      System.gc()
    }
  }

  private def processQuizItem(timeTakenToFindItem: Long,
      quizItem: QuizItemViewWithChoices, lastQuizItem: Option[QuizItemViewWithChoices],
      quiz: Quiz, qgWithHeader: QuizGroupWithHeader) (implicit maxResponses: Long) {

    output("Prompt: " + quizItem.prompt + "\tChoices: " + quizItem.allChoices.mkString(", "))

    val problem = findProblem(timeTakenToFindItem, quizItem, lastQuizItem, quiz)
    problem.foreach(output(_))
    if (!problem.isDefined) {
      val updatedQuiz = quiz.updatedPromptNumber(qgWithHeader)
      val simulatedResponse = makeResponse(quizItem, quiz)
      output("Simulated response: " + simulatedResponse)
      val quizAfterResponse = processUserResponse(updatedQuiz, simulatedResponse, quizItem)

      responsesProcessed += 1
      testWithQuizItems(quizAfterResponse, Some(quizItem))
    }
  }

  private def findProblem(timeTakenToFindItem: Long,
      quizItem: QuizItemViewWithChoices, lastQuizItem: Option[QuizItemViewWithChoices],
      quiz: Quiz): Option[String] =
    if (timeTakenToFindItem > 100 && responsesProcessed > 3)
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
  private def makeResponse(quizItem: QuizItemViewWithChoices, quiz: Quiz): String =
    quizItem.correctResponse.value

  private def processUserResponse(quiz: Quiz, userResponseTxt: String,
      quizItem: QuizItemViewWithChoices): Quiz = {
    val correctAnswer = quizItem.correctResponse
    val isCorrect = correctAnswer.matches(userResponseTxt)
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
    output("Score: " + formattedScore)
  }

  // Don't measure processing times while the system is still "warming up"
  private def shouldMeasureTime = responsesProcessed > 2
}
