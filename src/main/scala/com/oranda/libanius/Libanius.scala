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

import java.io.File
import java.lang.CharSequence
import java.lang.Runnable
import java.lang.System
import com.oranda.libanius.io.AndroidIO
import com.oranda.libanius.model.wordmapping.QuizItemViewWithOptions
import com.oranda.libanius.model.wordmapping.QuizOfWordMappings
import com.oranda.libanius.model.wordmapping.WordMappingGroupReadOnly
import com.oranda.libanius.model.UserAnswer
import com.oranda.libanius.util.Platform
import com.oranda.libanius.util.Util
import android.app.Activity
import android.content.Intent
import android.graphics.Color
import android.os.AsyncTask
import android.os.Bundle
import android.os.Handler
import android.view.View
import android.widget.Button
import android.widget.TextView
import scala.io.Source

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
  private[this] lazy val speedLabel: TextView = findView(TR.speed)
  private[this] lazy val statusLabel: TextView = findView(TR.status)
  
  private[this] var currentQuizItem: QuizItemViewWithOptions = _

  def quiz = GlobalState.quiz.get
  def dictionaryIsDefined = GlobalState.dictionary.isDefined
  def dictionary = GlobalState.dictionary.get
  
  override def onCreate(savedInstanceState: Bundle) {
    super.onCreate(savedInstanceState)
    log("Libanius", "onCreate")
    setContentView(R.layout.main) 
	initQuiz()
	loadDictionaryInBackground()
    testUserWithQuizItem
  }
  
  def initQuiz() {
    GlobalState.initQuiz(readQuizUi)
    val extrasOpt = Option(getIntent().getExtras()) // values from SearchDictionary activity
    extrasOpt.foreach { extras =>
	  val keyWord = extras.getString(Props.KEY_WORD)
      val value = extras.getString(Props.VALUE)
      log("Libanius", "received vars " + keyWord + " " + value)
      if (dictionaryIsDefined)
        quiz.addWordMappingToFrontOfTwoGroups(dictionary._keyType, 
            dictionary._valueType, keyWord, value)   
	}
  }
  
  def loadDictionaryInBackground() {
    val ctx = this
    new AsyncTask[Object, Object, Object] {      
      override def doInBackground(args: Object*): Object = {
        GlobalState.initDictionary(readDictionary(ctx))
	    None
      }
    }.execute()
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
    
    answerOption1Button.setBackgroundColor(Color.LTGRAY)
    answerOption2Button.setBackgroundColor(Color.LTGRAY)
    answerOption3Button.setBackgroundColor(Color.LTGRAY)

    questionLabel.setText(currentQuizItem.keyWord)
    var questionNotesText = "What is the " + currentQuizItem.valueType + "?"
    if (currentQuizItem.numCorrectAnswersInARow > 0)
      questionNotesText += " (correctly answered " + 
          currentQuizItem.numCorrectAnswersInARow + " times)"
    questionNotesLabel.setText(questionNotesText)
        
    val optionsIter = currentQuizItem.optionsInRandomOrder().iterator
    answerOption1Button.setText(optionsIter.next)
    answerOption2Button.setText(optionsIter.next)
    answerOption3Button.setText(optionsIter.next)
  }
  
  def answerOption1Clicked(v: View) { processUserAnswer(answerOption1Button) }
  def answerOption2Clicked(v: View) { processUserAnswer(answerOption2Button) }
  def answerOption3Clicked(v: View) { processUserAnswer(answerOption3Button) }
  
  def removeCurrentWord(v: View) {
    if (quiz.removeWordMappingValue(currentQuizItem.keyWord, 
        currentQuizItem.wordMappingValue, currentQuizItem.keyType, 
        currentQuizItem.valueType))
      printStatus("Deleted word " + currentQuizItem.keyWord)
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
    val maxAnswers = Props.NUM_CORRECT_ANSWERS_REQUIRED
    if (currentQuizItem.wordMappingValue.numCorrectAnswersInARow == maxAnswers)
      prevQuestionText += " (correct " + maxAnswers + " times -- COMPLETE)"
    prevQuestionLabel.setText(prevQuestionText)
    
    val answerOptionButtons = List(answerOption1Button, answerOption2Button, 
        answerOption3Button)
    val prevOptionsLabels = List(prevAnswerOption1Label, prevAnswerOption2Label,
        prevAnswerOption3Label)  
    val buttonsToLabels = answerOptionButtons zip prevOptionsLabels
    
    buttonsToLabels.foreach { buttonToLabel =>
      setPrevOptionsText(buttonToLabel._2, buttonToLabel._1.getText)
    }  
    
    val correctButton = answerOptionButtons.find(_.getText == correctAnswer).get
    
    buttonsToLabels.foreach { buttonToLabel =>
      setColorOnAnswer(buttonToLabel._1, buttonToLabel._2, correctButton, clickedButton)
    }  
    
    val delayMillis = if (isCorrect) 50 else 500
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
    new AsyncTask[Object, Object, String] {
      
      override def doInBackground(args: Object*): String = {
        val scoreSoFar = Util.stopwatch(quiz.scoreSoFar, "scoreSoFar")
        (scoreSoFar * 100).toString()
      }
      
      override def onPostExecute(strScore: String) {
        val strScoreMaxIndex = scala.math.min(strScore.length(), 6)
        printScore(strScore.substring(0, strScoreMaxIndex) + "%")
      }
    }.execute()
  }
  
  def showSpeed() {
    speedLabel.setText("Speed: " + answerSpeed + "/min")
  }  
  
  def printStatus(text: String) {
    statusLabel.setText(text)
  }
  
  def printScore(score: String) {
    statusLabel.setText("Score: " + score)
  }
}
