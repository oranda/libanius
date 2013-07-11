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

import java.lang.CharSequence
import java.lang.Runnable
import scala.concurrent.{ExecutionContext, Future}
import ExecutionContext.Implicits.global

import com.oranda.libanius.model.wordmapping.QuizItemViewWithOptions
import com.oranda.libanius.model.wordmapping.QuizOfWordMappings
import com.oranda.libanius.model.UserAnswer
import com.oranda.libanius.util.Platform
import com.oranda.libanius.util.Util
import android.app.Activity
import android.content.Intent
import android.graphics.Color
import android.os.Bundle
import android.os.Handler
import android.view.View
import android.widget.Button
import android.widget.TextView

class Libanius extends Activity with TypedActivity with Platform with DataStore 
    with Timestamps {

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
  def dictionaryIsDefined = GlobalState.dictionary.isDefined
  def dictionary = GlobalState.dictionary.get
  
  override def onCreate(savedInstanceState: Bundle) {
    super.onCreate(savedInstanceState)
    Conf.setUp()
    log("Libanius", "onCreate")
    setContentView(R.layout.main)
    initQuiz()
    loadDictionaryInBackground()
    testUserWithQuizItem()
  }
  
  def initQuiz() {
    GlobalState.initQuiz(readQuizUi)
    val extras = Option(getIntent().getExtras()) // values from the SearchDictionary Activity
    extras.foreach { extras =>
	    val keyWord = extras.getString(Conf.conf.keyWord)
      val value = extras.getString(Conf.conf.value)
      log("Libanius", "received vars " + keyWord + " " + value)
      if (dictionaryIsDefined)
        updateQuiz(quiz.addWordMappingToFrontOfTwoGroups(dictionary.keyType, 
            dictionary.valueType, keyWord, value))   
	  }
  }
  
  def updateQuiz(newQuiz: QuizOfWordMappings) {
    GlobalState.quiz = Some(newQuiz)
  }
  
  def loadDictionaryInBackground() {
    val ctx = this

    /*
     * Instead of using Android's AsyncTask for a background task, use a Future.
     * The Future has no result. An Akka actor might be used instead later.
     */
    Future(GlobalState.initDictionary(readDictionary(ctx)))
  }

  override def onPause() {
    super.onPause()
    log("Libanius", "onPause")
    saveQuizUi
  }
  
  def readQuizUi: QuizOfWordMappings = {    
    printStatus("Reading quiz data...")
    val quiz = readQuiz(this)
    val msg = "Finished reading " + quiz.numItems + " quiz items!"
    log("Libanius", msg)
    printStatus(msg)
    quiz
  }
  
  def testUserWithQuizItem() { 
    Util.stopwatch(quiz.findQuizItem, "find quiz item") match {
      case Some(quizItem) =>       
        currentQuizItem = quizItem
        showNextQuizItem()
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
    quiz.incPromptNumber

    answerOptionButtons.foreach(_.setBackgroundColor(Color.LTGRAY))

    questionLabel.setText(currentQuizItem.keyWord)
    var questionNotesText = "What is the " + currentQuizItem.valueType + "?"
    if (currentQuizItem.numCorrectAnswersInARow > 0)
      questionNotesText += " (correctly answered " + 
          currentQuizItem.numCorrectAnswersInARow + " times)"
    questionNotesLabel.setText(questionNotesText)
        
    val optionsIter = currentQuizItem.optionsInRandomOrder().iterator
    answerOptionButtons.foreach(_.setText(optionsIter.next))
  }
  
  def answerOption1Clicked(v: View) { processUserAnswer(answerOption1Button) }
  def answerOption2Clicked(v: View) { processUserAnswer(answerOption2Button) }
  def answerOption3Clicked(v: View) { processUserAnswer(answerOption3Button) }
  
  def removeCurrentWord(v: View) {
    val (newQuiz, wasRemoved) = quiz.removeWordMappingValue(currentQuizItem.keyWord, 
        currentQuizItem.wordMappingValue, currentQuizItem.keyType, currentQuizItem.valueType)
        
    updateQuiz(newQuiz)
    
    if (wasRemoved) printStatus("Deleted word " + currentQuizItem.keyWord)
    testUserWithQuizItemAgain()    
  }
  
  def gotoDictionary(v: View) {
    val dictScreen = new Intent(getApplicationContext(), classOf[SearchDictionary])
    startActivity(dictScreen)
  }
  
  def processUserAnswer(clickedButton: Button) {
    val userAnswerTxt = clickedButton.getText.toString
    val correctAnswer = currentQuizItem.wordMappingValue.value 
    val isCorrect = userAnswerTxt == correctAnswer
    updateTimestamps(isCorrect)
      
    val userAnswer = new UserAnswer(isCorrect, quiz.currentPromptNumber)
    currentQuizItem.wordMappingValue.addUserAnswer(userAnswer)
    
    var prevQuestionText = "PREV: " + questionLabel.getText
    val maxAnswers = Conf.conf.numCorrectAnswersRequired
    if (currentQuizItem.wordMappingValue.numCorrectAnswersInARow == maxAnswers)
      prevQuestionText += " (correct " + maxAnswers + " times -- COMPLETE)"
    prevQuestionLabel.setText(prevQuestionText)

    val buttonsToLabels = answerOptionButtons zip prevOptionLabels
    
    buttonsToLabels.foreach { buttonToLabel =>
      setPrevOptionsText(buttonToLabel._2, buttonToLabel._1.getText)
    }  
    
    val correctButton = answerOptionButtons.find(_.getText == correctAnswer).get
    
    buttonsToLabels.foreach { buttonToLabel =>
      setColorOnAnswer(buttonToLabel._1, buttonToLabel._2, correctButton, clickedButton)
    }  
    
    val delayMillis = if (isCorrect) 10 else 300
    val handler = new Handler
    handler.postDelayed(new Runnable() { def run() = testUserWithQuizItemAgain() }, 
        delayMillis)
  }
  
  def setPrevOptionsText(prevOptionLabel: TextView, keyWord: CharSequence) {
    val keyWordStr = keyWord.toString
    // The arguments for quiz.findValuesFor() have keyType and valueType reversed
    val values = quiz.findValuesFor(keyWordStr, 
        valueType = currentQuizItem.keyType, 
        keyType = currentQuizItem.valueType).mkString(", ")
    prevOptionLabel.setText(keyWord.toString + " = " + values)
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
        answerOptionButton.setBackgroundColor(Color.LTGRAY)
        prevAnswerOptionLabel.setTextColor(Color.LTGRAY)
    }
  }
  
  def saveQuizUi() {
    printStatus("Saving quiz data...")
    saveQuiz(this, quiz)
    printStatus("Finished saving quiz data!")
  }  
  
  def showScoreAsync() {

    def scoreSoFarStr = (Util.stopwatch(quiz.scoreSoFar, "scoreSoFar") * 100).toString
    /*
     * Instead of using Android's AsyncTask, use a Scala Future. It's more concise and general,
     * but we need to remind Android to use the UI thread when the result is returned.
     */
    val future = Future(scoreSoFarStr)

    def formatAndPrintScore(scoreStr: String) {
      val scoreStrMaxIndex = scala.math.min(scoreStr.length, 6)
      printScore(scoreStr.substring(0, scoreStrMaxIndex) + "%")
    }

    future.foreach(scoreSoFarStr =>
        runOnUiThread(new Runnable { override def run() { formatAndPrintScore(scoreSoFarStr) } }))
  }
  
  def showSpeed() { speedLabel.setText("Speed: " + answerSpeed + "/min") }
  
  def printStatus(text: String) { statusLabel.setText(text) }
  
  def printScore(score: String) { statusLabel.setText("Score: " + score) }
}
