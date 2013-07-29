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
package com.oranda.libanius

import java.lang.Runnable
import scala.concurrent.{ future, ExecutionContext }
import ExecutionContext.Implicits.global

import com.oranda.libanius.model.wordmapping.{QuizGroupHeader, QuizItemViewWithOptions, QuizOfWordMappings}
import com.oranda.libanius.util.Util
import android.app.Activity
import android.content.Intent
import android.graphics.Color
import android.os.Bundle
import android.os.Handler
import android.view.View
import android.widget.Button
import android.widget.TextView

class QuizScreen extends Activity with TypedActivity with DataStore with Timestamps {

  private[this] lazy val questionLabel: TextView = findView(TR.question)
  private[this] lazy val questionNotesLabel: TextView = findView(TR.questionNotes)
  private[this] lazy val answerOption1Button: Button = findView(TR.answerOption1)
  private[this] lazy val answerOption2Button: Button = findView(TR.answerOption2)
  private[this] lazy val answerOption3Button: Button = findView(TR.answerOption3)
    
  private[this] lazy val prevQuestionLabel: TextView = findView(TR.prevQuestion)
  private[this] lazy val prevAnswerOption1Label: TextView = findView(TR.prevAnswerOption1)
  private[this] lazy val prevAnswerOption2Label: TextView = findView(TR.prevAnswerOption2)
  private[this] lazy val prevAnswerOption3Label: TextView = findView(TR.prevAnswerOption3)

  private lazy val answerOptionButtons = List(answerOption1Button, answerOption2Button,
      answerOption3Button)
  private lazy val prevOptionLabels = List(prevAnswerOption1Label, prevAnswerOption2Label,
      prevAnswerOption3Label)

  private[this] lazy val speedLabel: TextView = findView(TR.speed)
  private[this] lazy val statusLabel: TextView = findView(TR.status)
  
  private[this] var currentQuizItem: QuizItemViewWithOptions = _

  def quiz = GlobalState.quiz.get
  //def dictionaryIsDefined = GlobalState.dictionary.isDefined
  //def dictionary = GlobalState.dictionary.get
  
  override def onCreate(savedInstanceState: Bundle) {
    super.onCreate(savedInstanceState)
    log("onCreate")
    setContentView(R.layout.quizscreen)
    initQuiz()
    //loadDictionaryInBackground()

    log("quiz groups are " + quiz.wordMappingGroups.map(_.header).mkString)
    testUserWithQuizItem()
  }
  
  def initQuiz() {
    /*
    val extras = Option(getIntent().getExtras()) // values from the OptionsScreen Activity
    extras.foreach { extras =>
	    val keyWord = extras.getString(Conf.conf.keyWord)
      val value = extras.getString(Conf.conf.value)
      log("received vars " + keyWord + " " + value)
      if (dictionaryIsDefined)
        GlobalState.updateQuiz(quiz.addWordMappingToFrontOfTwoGroups(
            QuizGroupHeader(dictionary.keyType, dictionary.valueType), keyWord, value))
	  }
	  */
  }


  //def loadDictionaryInBackground() =
  //  /*
  //   * Instead of using Android's AsyncTask for a background task, use a Future.
  //   * The Future has no result. An Akka actor might be used instead later.
  //   */
  //  future { GlobalState.initDictionary(readDictionary(ctx = this)) }

  override def onPause() {
    super.onPause()
    log("onPause")
    saveQuiz
  }

  
  def testUserWithQuizItem() { 
    Util.stopwatch(quiz.findQuizItem, "find quiz items") match {
      case Some((quizItem, wmg)) =>
        currentQuizItem = quizItem
        showNextQuizItem()
        GlobalState.quiz.foreach { q =>
          GlobalState.updateQuiz(
              q.addWordMappingGroup(wmg.updatedPromptNumber).updateRangeForFailedWmgs(wmg))
          q.wordMappingGroups.foreach(wmg => log(wmg.keyType + " prompt number is " +
              wmg.currentPromptNumber + ", range is " + wmg.currentSearchRange.start))
        }
      case _ =>
        printStatus("No more questions found! Done!")
    }
  }
  
  def testUserWithQuizItemAgain() { 
    showScoreAsync() // The score takes a second to calculate, so do it in the background
    showSpeed()
    testUserWithQuizItem()
  }
  
  def showNextQuizItem() {
    answerOptionButtons.foreach(_.setBackgroundColor(Color.LTGRAY))

    questionLabel.setText(currentQuizItem.keyWord)
    var questionNotesText = "What is the " + currentQuizItem.valueType + "?"
    if (currentQuizItem.numCorrectAnswersInARow > 0)
      questionNotesText += " (correctly answered " +
          currentQuizItem.numCorrectAnswersInARow + " times)"
    questionNotesLabel.setText(questionNotesText)
        
    val optionsIter = currentQuizItem.allOptions.iterator
    answerOptionButtons.foreach(_.setText(optionsIter.next))
  }
  
  def answerOption1Clicked(v: View) { processUserAnswer(answerOption1Button) }
  def answerOption2Clicked(v: View) { processUserAnswer(answerOption2Button) }
  def answerOption3Clicked(v: View) { processUserAnswer(answerOption3Button) }
  
  def removeCurrentWord(v: View) {
    val (newQuiz, wasRemoved) = quiz.removeWordMappingValue(currentQuizItem.keyWord, 
        currentQuizItem.wordMappingValue, currentQuizItem.quizGroupHeader)
        
    GlobalState.updateQuiz(newQuiz)
    
    if (wasRemoved) printStatus("Deleted word " + currentQuizItem.keyWord)
    testUserWithQuizItemAgain()    
  }
  
  def gotoDictionary(v: View) {
    val dictScreen = new Intent(getApplicationContext(), classOf[OptionsScreen])
    startActivity(dictScreen)
  }

  private def updateUI(correctAnswer: String, clickedButton: Button) {
    resetButtonAndLabelColors()
    setPrevQuestionText()
    populatePrevOptions()
    setColorsForButtons(correctAnswer, clickedButton)
  }

  private def resetButtonAndLabelColors() {
    prevOptionLabels.foreach (_.setTextColor(Color.LTGRAY))
    answerOptionButtons.foreach (_.setBackgroundColor(Color.LTGRAY))
  }

  private def setPrevQuestionText() {
    var prevQuestionText = "PREV: " + questionLabel.getText
    val maxAnswers = Conf.conf.numCorrectAnswersRequired
    if (currentQuizItem.wordMappingValue.numCorrectAnswersInARow == maxAnswers)
      prevQuestionText += " (correct " + maxAnswers + " times -- COMPLETE)"
    prevQuestionLabel.setText(prevQuestionText)
  }

  private def populatePrevOptions() {

    log("allOptions for currentQuizItem: " + currentQuizItem.allOptions)

    val reverseGroupHeader = currentQuizItem.quizGroupHeader.reverse
    val isReverseLookupPossible = (GlobalState.quiz.flatMap(
        _.findWordMappingGroup(reverseGroupHeader)
    )).isDefined

    if (isReverseLookupPossible) {
      val labelsToOptions = prevOptionLabels zip currentQuizItem.allOptions
      labelsToOptions.foreach {
        case (label, option) => setPrevOptionsText(label, option, reverseGroupHeader)
      }
    } else {
      prevAnswerOption1Label.setTextColor(Color.GREEN)
      prevAnswerOption1Label.setText(currentQuizItem.keyWord + " = " +
          currentQuizItem.wmvs.strings.mkString(", "))
    }
  }

  private def setColorsForButtons(correctAnswer: String, clickedButton: Button) {

    val correctButton = answerOptionButtons.find(_.getText == correctAnswer).get

    val buttonsToLabels = answerOptionButtons zip prevOptionLabels
    buttonsToLabels.foreach { buttonToLabel =>
      setColorOnAnswer(buttonToLabel._1, buttonToLabel._2, correctButton, clickedButton)
    }
  }

  def setPrevOptionsText(prevOptionLabel: TextView, keyWord: String,
      quizGroupHeader: QuizGroupHeader) {

    log("quiz wordMappingGroups: " + quiz.wordMappingGroups.map(_.header))
    // The arguments for quiz.findValuesFor() have keyType and valueType reversed
    val values = quiz.findValuesFor(keyWord, quizGroupHeader).mkString(", ")
    prevOptionLabel.setText(keyWord + " = " + values)
  }
  
  def setColorOnAnswer(answerOptionButton: Button, 
      prevAnswerOptionLabel: TextView, CORRECT_BUTTON: Button, CLICKED_BUTTON: Button) {
    
    answerOptionButton match {
      case CORRECT_BUTTON => 
        answerOptionButton.setBackgroundColor(Color.GREEN)
        prevAnswerOptionLabel.setTextColor(Color.GREEN)
      case CLICKED_BUTTON => 
        answerOptionButton.setBackgroundColor(Color.RED)
        prevAnswerOptionLabel.setTextColor(Color.RED)
      case _ =>
    }
  }
  
  def saveQuiz() {
    printStatus("Saving quiz data...")
    saveWmgs(quiz, ctx = Some(this))
    printStatus("Finished saving quiz data!")
  }

  def processUserAnswer(clickedButton: Button) {
    val userAnswerTxt = clickedButton.getText.toString
    val correctAnswer = currentQuizItem.wordMappingValue.value
    val isCorrect = userAnswerTxt == correctAnswer
    updateTimestamps(isCorrect)

    GlobalState.updateQuiz(Util.stopwatch(
      GlobalState.quiz.get.updateWithUserAnswer(isCorrect, currentQuizItem), "updateQuiz"))
    updateUI(correctAnswer, clickedButton)

    val delayMillis = if (isCorrect) 10 else 300
    val handler = new Handler
    handler.postDelayed(new Runnable() { def run() = testUserWithQuizItemAgain() },
      delayMillis)
  }

  def showScoreAsync() {

    def formatAndPrintScore(scoreStr: String) {
      val scoreStrMaxIndex = scala.math.min(scoreStr.length, 6)
      printScore(scoreStr.substring(0, scoreStrMaxIndex) + "%")
    }

    /*
     * Instead of using Android's AsyncTask, use a Scala Future. It's more concise and general,
     * but we need to remind Android to use the UI thread when the result is returned.
     */
    future {
      (Util.stopwatch(quiz.scoreSoFar, "scoreSoFar") * 100).toString
    } map { scoreSoFar: String =>
        runOnUiThread(new Runnable { override def run() { formatAndPrintScore(scoreSoFar) } })
    }
  }
  
  def showSpeed() { speedLabel.setText("Speed: " + answerSpeed + "/min") }
  
  def printStatus(text: String) { statusLabel.setText(text) }
  
  def printScore(score: String) { statusLabel.setText("Score: " + score) }
}
