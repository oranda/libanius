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

import android.app.{AlertDialog, Activity}
import android.widget._
import android.os.Bundle
import android.view.{ViewGroup, View}
import android.content.{Context, Intent}
import scala.concurrent.{Await, Future, ExecutionContext}
import ExecutionContext.Implicits.global
import com.oranda.libanius.model.wordmapping.{QuizOfWordMappings, WordMappingGroupReadWrite, QuizGroupHeader}
import scala.collection.immutable.Set
import scala.concurrent.duration._
import java.util.concurrent.TimeoutException
import android.widget.CompoundButton.OnCheckedChangeListener

class OptionsScreen extends Activity with TypedActivity with DataStore {

  private[this] lazy val quizGroupLayout: LinearLayout = findView(TR.checkboxesLayout)

  /*
  private[this] lazy val searchInputBox: EditText = findView(TR.searchInput) 
  private[this] lazy val searchResults0Row: LinearLayout = findView(TR.searchResults0)
  private[this] lazy val searchResults1Row: LinearLayout = findView(TR.searchResults1)
  private[this] lazy val searchResults2Row: LinearLayout = findView(TR.searchResults2)
  private[this] lazy val status: TextView = findView(TR.status)
  */
  private var checkBoxes = Map[CheckBox, QuizGroupHeader]()
  private var wmgLoadingFutures: Set[Future[WordMappingGroupReadWrite]] = Set()

  override def onCreate(savedInstanceState: Bundle) {
    super.onCreate(savedInstanceState)
    Conf.setUp()
    log("OptionsScreen.onCreate")
    initGui()
  }

  def initGui() {
    setContentView(R.layout.optionsscreen)
    addQuizGroupCheckBoxes()
    //prepareSearchUi()
  }

  def addQuizGroupCheckBoxes() {

    val activeHeaders = activeQuizGroupHeaders
    log("activeHeaders: " + activeHeaders)

    def makeQuizGroupCheckBox(ctx: Context, quizGroupHeader: QuizGroupHeader): CheckBox = {
      val checkBox = new CheckBox(getApplicationContext)
      checkBox.setText(quizGroupHeader.toString)
      checkBox.setOnCheckedChangeListener(new OnCheckedChangeListener() {
        override def onCheckedChanged(buttonView: CompoundButton, isChecked: Boolean) {
          if (isChecked)
            wmgLoadingFutures += loadWmg(ctx, quizGroupHeader)
        }
      })

      if (activeHeaders.contains(quizGroupHeader)) {
        checkBox.setChecked(true)
        log("setChecked " + quizGroupHeader)
      }
      checkBox
    }

    def addCheckBoxToLayout(checkBox: CheckBox) = {
      val params = new LinearLayout.LayoutParams(ViewGroup.LayoutParams.FILL_PARENT,
          ViewGroup.LayoutParams.WRAP_CONTENT)
      params.leftMargin = 20
      params.bottomMargin = 25
      quizGroupLayout.addView(checkBox, params)
    }

    val availableWmgs = findAvailableWmgs(ctx = this)
    availableWmgs.foreach(wmgHeader => log("wmg: " + wmgHeader))
    checkBoxes = availableWmgs.map(wmgHeader =>
      (makeQuizGroupCheckBox(ctx = this, wmgHeader), wmgHeader)).toMap

    checkBoxes.keys.foreach(addCheckBoxToLayout(_))
  }

  def noBoxesChecked = checkBoxes.filter(_._1.isChecked).isEmpty

  def checkedQuizGroupHeaders: Set[QuizGroupHeader] =
    checkBoxes.filter(_._1.isChecked).map(_._2).toSet

  def activeQuizGroupHeaders: Set[QuizGroupHeader] = {
    def groupHeaders(quiz: QuizOfWordMappings) = quiz.wordMappingGroups.map(_.header)
    (GlobalState.quiz.map(groupHeaders(_))).getOrElse(readQuizMetadata(ctx = this))
  }

  def alert(title: String, message: String) {
    new AlertDialog.Builder(OptionsScreen.this).setTitle(title).setMessage(message).
        setPositiveButton("OK", null).show()
  }

  def gotoQuiz(v: View) {
    if (noBoxesChecked)
      alert("Error", "No boxes checked")
    else {
      try {
        Await.result(Future.sequence(wmgLoadingFutures), 10 seconds)
      } catch {
        case e: TimeoutException => log("ERROR: Timed out loading quiz groups")
      }
      // TODO: rather than having a general Await as above, have individual waits for each WMG
      fillQuizWithCheckedQuizGroups()
      val intent = new Intent(getBaseContext(), classOf[QuizScreen])
      startActivity(intent)
    }
  }

  def fillQuizWithCheckedQuizGroups() {

    def quizGroupForHeader(header: QuizGroupHeader): Option[WordMappingGroupReadWrite] =
      GlobalState.loadedQuizGroups.find(_.header == header)

    val checkedQuizGroups = checkedQuizGroupHeaders.flatMap(quizGroupForHeader(_))
    log("filling quiz with checkedQuizGroups " + checkedQuizGroups.map(_.header))

    GlobalState.updateQuiz(QuizOfWordMappings(checkedQuizGroups))
  }

  /*

  def prepareSearchUi() {
    searchInputBox.setOnEditorActionListener(new TextView.OnEditorActionListener() {
      override def onEditorAction(searchInputBox: TextView, actionId: Int,
                                  event: KeyEvent): Boolean = {
        if (actionId == EditorInfo.IME_ACTION_DONE || event.getAction() == KeyEvent.ACTION_DOWN)
          findAndShowResultsAsync()
        true
      }
    })
  }

  def findAndShowResultsAsync() {
    clearResults()
    status.setText("Searching...")
    val searchInput = searchInputBox.getText.toString

    /*
     * Instead of using Android's AsyncTask, use a Scala Future. It's more concise and general,
     * but we need to remind Android to use the UI thread when the result is returned.
     */
    future {
      Util.stopwatch(searchDictionary(searchInput), "search dictionary")
    } map { searchResults =>
      runOnUiThread(new Runnable { override def run() { showSearchResults(searchResults) } })
    }

    def showSearchResults(searchResults: List[(String, String)]) {
      if (searchResults.isEmpty)
        status.setText("No results found")
      else {
        status.setText("")
        addRow(searchResults0Row, searchResults, 0)
        addRow(searchResults1Row, searchResults, 1)
        addRow(searchResults2Row, searchResults, 2)
      }
    }
  }

  def searchDictionary(searchInput: String): List[(String, String)] = {
        
 	  def resultsBeginningWith(input: String): List[(String, String)] =
	    GlobalState.dictionary.get.mappingsForKeysBeginningWith(input)
	  
    var searchResults = List[(String, String)]()
    if (searchInput.length > 2 && GlobalState.dictionary.isDefined) {
      searchResults = resultsBeginningWith(searchInput)
      if (searchResults.isEmpty)
        searchResults = resultsBeginningWith(searchInput.dropRight(1))
      if (searchResults.isEmpty)
        searchResults = resultsBeginningWith(searchInput.dropRight(2))
      if (searchResults.isEmpty && searchInput.length > 3)
        searchResults = GlobalState.dictionary.get.mappingsForKeysContaining(
	          searchInput)
	  }
 	  searchResults
  }
  
  def addRow(searchResultsRow: LinearLayout, searchResults: List[(String, String)], index: Int) {
    if (searchResults.size > index) {
      val keyWordBox = new TextView(this)
      keyWordBox.setLayoutParams(new LayoutParams(LayoutParams.WRAP_CONTENT,
          LayoutParams.WRAP_CONTENT))
      val keyWord = searchResults(index)._1
      keyWordBox.setText(keyWord)
      searchResultsRow.addView(keyWordBox)
             
      val maxNumButtons = 5
      val values = searchResults(index)._2.split("/").slice(0, maxNumButtons)
      values.foreach { value =>
        val btnTag = new Button(this)
        btnTag.setLayoutParams(new LayoutParams(LayoutParams.WRAP_CONTENT, 
            LayoutParams.WRAP_CONTENT))
        btnTag.setText(value)
        btnTag.setOnClickListener(new OnClickListener() {
          def onClick(view: View) {
            val intent = new Intent(getBaseContext(), classOf[QuizScreen])
            intent.putExtra("keyWord", keyWord)
            intent.putExtra("value", value)
            startActivity(intent)
          }
        })
        searchResultsRow.addView(btnTag)
      }
    }     
  }

  def clearResults() {
    status.setText("")
    searchResults0Row.removeAllViews()
    searchResults1Row.removeAllViews()
    searchResults2Row.removeAllViews()
  }
  */
}