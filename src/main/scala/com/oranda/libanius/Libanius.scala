/*
 * Copyright 2012 James McCabe <james@oranda.com>
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
import java.lang.System
import com.oranda.libanius.io.AndroidIO
import com.oranda.libanius.model.wordmapping.QuizItemViewWithOptions
import com.oranda.libanius.model.wordmapping.QuizOfWordMappings
import com.oranda.libanius.model.UserAnswer
import com.oranda.libanius.util.Platform
import com.oranda.libanius.util.Util
import android.app.Activity
import android.graphics.Color
import android.os.Bundle
import android.os.Handler
import android.util.Log
import android.view.View
import android.widget.Button
import android.widget.TextView

class Libanius extends Activity with TypedActivity {

  private[this] var questionLabel : TextView = _
  private[this] var questionNotesLabel : TextView = _
  private[this] var answerOption1Button : Button = _
  private[this] var answerOption2Button : Button = _
  private[this] var answerOption3Button : Button = _
    
  private[this] var prevQuestionLabel : TextView = _
  private[this] var prevAnswerOption1Label : TextView = _
  private[this] var prevAnswerOption2Label : TextView = _
  private[this] var prevAnswerOption3Label : TextView = _
  private[this] var speedLabel : TextView = _
  private[this] var statusLabel : TextView = _

  private[this] var quiz: QuizOfWordMappings = new QuizOfWordMappings
  private[this] var currentQuizItem: QuizItemViewWithOptions = _

  private[this] var timestampsLastCorrectAnswers = List[Long]()
  
  override def onCreate(savedInstanceState: Bundle) {
    super.onCreate(savedInstanceState)
	initGui
	if (quiz.numGroups == 0)
      quiz = readQuiz    
    testUserWithQuizItem 
  }
  
  override def onPause() {
    super.onPause()
    Platform.log("Libanius", "onPause")
    saveQuiz
  }
  
  def initGui {
    
    setContentView(R.layout.main)
    questionLabel = findView(TR.question)
    questionNotesLabel = findView(TR.questionNotes)
    answerOption1Button = findView(TR.answerOption1)
    answerOption2Button = findView(TR.answerOption2)
    answerOption3Button = findView(TR.answerOption3)
    speedLabel = findView(TR.speed)
    statusLabel = findView(TR.status)
    
    prevQuestionLabel = findView(TR.prevQuestion)
    prevAnswerOption1Label = findView(TR.prevAnswerOption1)
    prevAnswerOption2Label = findView(TR.prevAnswerOption2)
    prevAnswerOption3Label = findView(TR.prevAnswerOption3)
    
  }
  
  def readQuiz : QuizOfWordMappings = {
      
    printStatus("Reading quiz data...")
    val fileText = AndroidIO.readFile(this, Props.fileQuiz)
      
    val quiz = Util.stopwatch(QuizOfWordMappings.fromCustomFormat(fileText), 
        "reading and parsing quiz")      
    
    printStatus("Finished reading " + quiz.numItems + " quiz items!")
    return quiz
  }
  
  
  def testUserWithQuizItem() { 
    
    val quizItemOpt = Util.stopwatch(quiz.findQuizItem, "find quiz item")
    
    if (!quizItemOpt.isDefined)      
      printStatus("No more questions found! Done!")
    else {
      currentQuizItem = quizItemOpt.get
      showNextQuizItem()
    } 
  }
  
  def testUserWithQuizItemAgain() { 
    showScore()
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
      questionNotesText += " (already answered correctly " + 
          currentQuizItem.numCorrectAnswersInARow + " times)"
    questionNotesLabel.setText(questionNotesText)
        
    val optionsIter = currentQuizItem.optionsInRandomOrder().iterator
    answerOption1Button.setText(optionsIter.next)
    answerOption2Button.setText(optionsIter.next)
    answerOption3Button.setText(optionsIter.next)
    
    //if (quiz.currentPromptNumber % 5000 == 0)
    //  saveQuiz()
  }
  
  def answerOption1Clicked(v: View) { processUserAnswer(answerOption1Button) }
  def answerOption2Clicked(v: View) { processUserAnswer(answerOption2Button) }
  def answerOption3Clicked(v: View) { processUserAnswer(answerOption3Button) }
  
  def deleteCurrentWord(v: View) {
    if (quiz.deleteWordMappingValue(currentQuizItem.keyWord, 
        currentQuizItem.wordMappingValue, currentQuizItem.keyType, 
        currentQuizItem.valueType))
      printStatus("Deleted word " + currentQuizItem.keyWord)
    testUserWithQuizItemAgain()    
  }
  
  def updateTimestamps(thereJustOccurredACorrectAnswer: Boolean) {
    if (thereJustOccurredACorrectAnswer) {
      val currentTime = System.currentTimeMillis
      timestampsLastCorrectAnswers :+= currentTime
      /* 
       * Purge timestamps older than one minute. This leaves the length of the 
       * list as a measure of the number of correct answers per minute.
       */
      timestampsLastCorrectAnswers = timestampsLastCorrectAnswers.filter(
          timestamp => timestamp > currentTime - 60000)
    } 
  }
  
  def answerSpeed = timestampsLastCorrectAnswers.size
  
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
    
    for (buttonToLabel <- buttonsToLabels)
      setPrevOptionsText(buttonToLabel._2, buttonToLabel._1.getText)
    
    val correctButton = answerOptionButtons.find(_.getText == correctAnswer).get
    
    for (buttonToLabel <- buttonsToLabels)
      setColorOnAnswer(buttonToLabel._1, buttonToLabel._2, correctButton, clickedButton)
      
    
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
  
  def saveQuiz() {
    printStatus("Saving quiz data...")
    val str = Util.stopwatch(quiz.toCustomFormat, "serialize the quiz")
    AndroidIO.save(this, Props.fileQuiz, Props.fileQuizLastBackup, str.toString)
    printStatus("Finished saving quiz data!")
  }  
  
  def showScore() {
    val scoreSoFar = Util.stopwatch(quiz.scoreSoFar, "scoreSoFar")
    val quizStr = (scoreSoFar * 100).toString()
    val quizStrMaxIndex = scala.math.min(quizStr.length(), 6)
    printScore(quizStr.substring(0, quizStrMaxIndex) + "%")
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
