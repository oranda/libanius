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

package com.oranda.libanius.model.questions

import org.specs2.mutable.Specification
import com.oranda.libanius.Conf

class QuestionItemSpec extends Specification {

  "a question item" should {
    
    val qiXml = 
<quizItem>
  <question>What is the name for the system of linked documents on the Internet?</question>
  <answer>(The World Wide) Web</answer>
  <userAnswers></userAnswers>
</quizItem>

    Conf.setUpDummy()
    
    val qi = QuestionItem.fromXML(qiXml)
    
    sequential 
    
    "be parseable from XML" in {
      qi.question mustEqual "What is the name for the system of linked documents on the Internet?"
      qi.correctAnswer mustEqual "(The World Wide) Web"
      qi.userAnswers.length mustEqual 0
    }
    
    "accept answers as correct even if they do not include parenthesized text in the given answer" in {
      qi.isCorrect("Web") mustEqual true
      qi.isCorrect("World") mustEqual false
    }
    
  }
}