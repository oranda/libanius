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

package com.oranda.libanius.model.wordmapping

import org.specs2.mutable.Specification
import com.oranda.libanius.model.{UserResponses, Criteria}
import com.oranda.libanius.dependencies.{AppDependencies, Conf}

class WordMappingValueSpec extends Specification {
  
  "a word-mapping-response" should {

    AppDependencies.conf = Conf.setUpForTest()

    val wmvCustomFormat = "nachlösen:9,7;6"

    val wmv = WordMappingValue.fromCustomFormat(wmvCustomFormat)
    
    "be parseable from custom format" in {
      wmv.value mustEqual "nachlösen"
      wmv.userAnswers.length mustEqual 3
      wmv.numCorrectAnswersInARow mustEqual 2
      wmv.toCustomFormat(new StringBuilder()).toString mustEqual wmvCustomFormat
    }
  }
  
}