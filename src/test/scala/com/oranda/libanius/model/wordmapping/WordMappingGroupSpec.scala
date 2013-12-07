/* Copyright 2012-2013 James McCabe <james@oranda.com>
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
import com.oranda.libanius.dependencies.AppDependencyAccess
import com.oranda.libanius.model.quizitem.QuizItem
import com.oranda.libanius.model.quizgroup.{QuizGroupUserData, WordMapping, QuizGroupHeader, QuizGroup}

class WordMappingGroupSpec extends Specification with AppDependencyAccess {
  
  "a word-mapping group" should {

    // main test data

    val wmgCustomFormat =
        "quizGroup type=\"WordMapping\" promptType=\"English word\" responseType=\"German word\" currentPromptNumber=\"0\"\n" +
        "against|wider\n" +
        "entertain|unterhalten\n" +
        "teach|unterrichten\n" +
        "winner|Siegerin\n" +
        "en route|unterwegs\n" +
        "full|satt/voll\n" +
        "interrupted|unterbrochen\n" +
        "contract|Vertrag\n" +
        "rides|reitet\n" +
        "sweeps|streicht"

    // test data for conversions with QuizGroup

    val header = QuizGroupHeader(WordMapping, "English word", "German word")
    val wmPairs = List[WordMappingPair](
      WordMappingPair("full", WordMappingValueSet.createFromStrings("voll", "satt")),
      WordMappingPair("against", WordMappingValueSet.createFromStrings("wider"))
    )
    val quizItems = List[QuizItem](
      QuizItem("full", "voll"),
      QuizItem("full", "satt"),
      QuizItem("against", "wider")
    )

    val wmgSmall = WordMappingGroup(header, wmPairs.toStream)
    val dictSmall = Dictionary.fromWordMappings(wmPairs.toStream)
    val quizGroup = QuizGroup(quizItems.toStream, QuizGroupUserData(), dictionary = dictSmall)

    "be convertible to a QuizGroup" in {
      wmgSmall.toQuizGroup mustEqual quizGroup
    }

    "be constructible from a QuizGroup" in {
      WordMappingGroup.fromQuizGroup(header, quizGroup) mustEqual wmgSmall
    }
  }
      
}