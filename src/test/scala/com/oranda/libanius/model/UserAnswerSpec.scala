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

import com.oranda.libanius.model.wordmapping.WordMappingValue
import org.specs2.mutable.Specification
import com.oranda.libanius.Props

class UserAnswerSpec extends Specification {

  "a user answer" should {
    
    Props.ANDROID = false
    
    val uaXml = <userAnswer wasCorrect="true" promptNumber="1"></userAnswer>
  
    val ua = UserAnswer.fromXML(uaXml)
    
    "be parseable from XML" in {
      ua.wasCorrect == true
      ua.promptNumber == 1
    }
  }
}