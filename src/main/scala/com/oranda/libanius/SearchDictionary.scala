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

import android.app.Activity
import android.widget.EditText
import android.os.Bundle
import android.view.inputmethod.EditorInfo
import android.widget.TextView
import android.view.KeyEvent
import com.oranda.libanius.model.wordmapping.WordMappingGroupReadOnly
import com.oranda.libanius.io.AndroidIO
import com.oranda.libanius.util.Platform
import com.oranda.libanius.util.Util
import android.widget.LinearLayout
import android.view.ViewGroup.LayoutParams
import android.widget.Button
import android.view.View.OnClickListener
import android.view.View
import android.content.Intent
import android.os.AsyncTask

class SearchDictionary extends Activity with TypedActivity with Platform {

  private[this] var searchInputBox: EditText = _
  private[this] var searchResults0Row: LinearLayout = _
  private[this] var searchResults1Row: LinearLayout = _
  private[this] var searchResults2Row: LinearLayout = _
  private[this] var status: TextView = _
  
  override def onCreate(savedInstanceState: Bundle) {
    super.onCreate(savedInstanceState)
	initGui
  }
  
  def initGui {    
    setContentView(R.layout.searchdictionary)
    searchInputBox = findView(TR.searchInput) 
    searchResults0Row = findView(TR.searchResults0)
    searchResults1Row = findView(TR.searchResults1)
    searchResults2Row = findView(TR.searchResults2)
    status = findView(TR.status)
    
    searchInputBox.setOnEditorActionListener(new TextView.OnEditorActionListener() {
      override def onEditorAction(searchInputBox: TextView, actionId: Int, 
          event: KeyEvent): Boolean = {
	    if (actionId == EditorInfo.IME_ACTION_DONE 
	        || event.getAction() == KeyEvent.ACTION_DOWN)
	      findAndShowResults()
	    true
	  }
    })
  }
  
  def findAndShowResults() {
    clearResults()
    status.setText("Searching...")
	val searchInput = searchInputBox.getText.toString
	
	new AsyncTask[Object, Object, List[(String, String)]] {
      
      override def doInBackground(args: Object*): List[(String, String)] = 
	    Util.stopwatch(searchDictionary(searchInput), "search dictionary")
      
      override def onPostExecute(searchResults: List[(String, String)]) {
	    if (searchResults.isEmpty)  
	      status.setText("No results found")
	    else {	      
          status.setText("")
	      addRow(searchResults0Row, searchResults, 0)
	      addRow(searchResults1Row, searchResults, 1)
	      addRow(searchResults2Row, searchResults, 2)
	    }
      }
    }.execute()
  }

  def searchDictionary(searchInput: String) = {
        
 	def resultsBeginningWith(input: String): List[(String, String)] = 
	  GlobalState.dictionary.get.mappingsForKeysBeginningWith(input)
	  
    var searchResults = List[(String, String)]()
	if (searchInput.length > 2 && GlobalState.dictionary.isDefined) { 
	  searchResults = resultsBeginningWith(searchInput)
	  if (searchResults.isEmpty)
	    searchResults = resultsBeginningWith(searchInput.dropRight(1))
	  if  (searchResults.isEmpty)
	    searchResults = resultsBeginningWith(searchInput.dropRight(2))
	  if (searchResults.isEmpty && searchInput.length > 3)
	    searchResults = GlobalState.dictionary.get.mappingsForKeysContaining(
	        searchInput)
	}    
 	searchResults
  }
  
  def addRow(searchResultsRow: LinearLayout, 
     searchResults: List[(String, String)], index: Int) {
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
            val intent = new Intent(getBaseContext(), classOf[Libanius])
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
  

}