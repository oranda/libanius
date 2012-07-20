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

package com.oranda.libanius.model.questions

import org.specs2.mutable.Specification
import com.oranda.libanius.Props


class QuizOfQuestionsSpec extends Specification {

  "a quiz of questions" should {
    
    val quizXml = 
<quiz>
  <currentPromptNumber>0</currentPromptNumber>
  <quizItems>
    <quizItem>
      <question>What is the English for wider?</question>
      <answer>against</answer>
      <userAnswers></userAnswers>
    </quizItem>
    <quizItem>
      <question>What is the German for against?</question>
      <answer>wider</answer>
      <userAnswers></userAnswers>
    </quizItem>
  </quizItems>
</quiz>
  
    Props.ANDROID = false
    
    val quiz = QuizOfQuestions.fromXML(quizXml)
     
    sequential 
    
    "be parseable from XML" in {
      quiz.currentPromptNumber mustEqual 0
      quiz.numQuestionItems mustEqual 2
    }
    
    "offer a presentable quiz item" in {
      val quizItem = quiz.findQuestionItem(numCorrectAnswersInARowDesired = 0, 
          diffInPromptNum = 0)
      quizItem.isDefined mustEqual true
    }
  }
}