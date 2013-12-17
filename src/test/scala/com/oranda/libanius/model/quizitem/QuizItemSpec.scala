package com.oranda.libanius.model.quizitem

import org.specs2.mutable.Specification
import com.oranda.libanius.dependencies.AppDependencyAccess
import com.oranda.libanius.model.{Criteria, UserResponses, UserResponse}

class QuizItemSpec extends Specification with AppDependencyAccess {

  "a quiz item" should {

    val correctAnswersInARow = List(UserResponse(9), UserResponse(7))
    val incorrectAnswers = List(UserResponse(6))
    val userResponses = UserResponses(correctAnswersInARow, incorrectAnswers)
    val quizItem = QuizItem(TextValue("solve"), TextValue("nachlösen"), userResponses)

    val quizItemCustomFormat = "solve|nachlösen|9,7;6"

    "be serializable to custom format" in {
      val customFormat = quizItem.toCustomFormat(new java.lang.StringBuilder(), "|")
      customFormat.toString mustEqual quizItemCustomFormat
    }

    "be parseable from custom format with the standard separator" in {
      val quizItemStr = "on|auf|"
      val quizItem = QuizItem.fromCustomFormat(quizItemStr)
      val quizItemExpected = QuizItem("on", "auf")
      quizItem mustEqual quizItemExpected
    }

    "be parseable from custom format with a special separator" in {
      val quizItemStr = "Given a String s = \"2.3\" convert it to a Double ||| s.toDouble"
      val quizItem = QuizItem.fromCustomFormat(quizItemStr, "|||")
      val quizItemExpected = QuizItem("Given a String s = \"2.3\" convert it to a Double",
          "s.toDouble")
      quizItem mustEqual quizItemExpected
    }

    def isPresentable(quizItem: QuizItem, numCorrectAnswersInARowDesired: Int,
        diffInPromptNumMinimum: Int, currentPromptNum: Int) = {
      val criteria = Criteria(numCorrectAnswersInARowDesired, diffInPromptNumMinimum)
      criteria.isPresentable(currentPromptNum, quizItem.promptNumInMostRecentAnswer,
          quizItem.numCorrectAnswersInARow)
    }

    "be presentable in the quiz, given certain criteria" in {

      isPresentable(quizItem, numCorrectAnswersInARowDesired = 2,
          diffInPromptNumMinimum = 2, currentPromptNum = 11) mustEqual true
      isPresentable(quizItem, numCorrectAnswersInARowDesired = 3,
          diffInPromptNumMinimum = 2, currentPromptNum = 11) mustEqual false
      isPresentable(quizItem, numCorrectAnswersInARowDesired = 2,
          diffInPromptNumMinimum = 3, currentPromptNum = 11) mustEqual false
      isPresentable(quizItem, numCorrectAnswersInARowDesired = 2,
          diffInPromptNumMinimum = 2, currentPromptNum = 10) mustEqual false
    }


    "be matchable against another by the first few letters" in {
      quizItem.correctResponse.hasSameStart("nachfahren")(4) mustEqual true
      quizItem.correctResponse.hasSameStart("nachfahren")(5) mustEqual false
    }

    "be matchable against another by the last few letters" in {
      quizItem.correctResponse.hasSameEnd("nachfahren")(2) mustEqual true
      quizItem.correctResponse.hasSameEnd("nachfahren")(3) mustEqual false
    }

    "know the number of times in a row it was answered correctly by the user" in {
      quizItem.numCorrectAnswersInARow must be equalTo 2
    }
  }
}
