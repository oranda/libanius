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

package com.oranda.libanius.model

import com.oranda.libanius.Props

case class UserAnswer(val wasCorrect: Boolean, val promptNumber: Int) extends ModelComponent {
  
  def toXML =
    <userAnswer wasCorrect={"" + wasCorrect} promptNumber={"" + promptNumber}/>
   
  // not currently used
  def toCustomFormat: String = wasCorrect + "_" + promptNumber
}

object UserAnswer {
    def fromXML(node: xml.Node): UserAnswer =
	  new UserAnswer(wasCorrect = (node \ "@wasCorrect").text.toBoolean,
	      promptNumber = (node \ "@promptNumber").text.toInt) 
    
    def fromCustomFormat(strCustomFormat: String): UserAnswer = {
        val parts = strCustomFormat.split("_")
        new UserAnswer(wasCorrect = parts(0).toBoolean,
            promptNumber = parts(1).toInt)
    }
}