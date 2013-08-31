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
import com.oranda.libanius.model._
import com.oranda.libanius.dependencies.{AppDependencies, Conf}

class WordMappingGroupSpec extends Specification {
  
  "a word-mapping group" should {

    // main test data

    val wmgCustomFormat =
        "quizGroup type=\"WordMapping\" keyType=\"English word\" valueType=\"German word\" currentPromptNumber=\"0\"\n" +
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

    AppDependencies.conf = Conf.setUpForTest()

    val wmg = WordMappingGroup.fromCustomFormat(wmgCustomFormat)

    // test data for conversions with QuizGroup

    val header = QuizGroupHeader(WordMapping, "English word", "German word")
    val wmPairs = List[WordMappingPair](
      WordMappingPair("full", WordMappingValueSet("voll", "satt")),
      WordMappingPair("against", WordMappingValueSet("wider"))
    )
    val quizPairs = List[QuizPair](
      QuizPair("full", "voll"),
      QuizPair("full", "satt"),
      QuizPair("against", "wider")
    )

    val wmgSmall = WordMappingGroup(header, wmPairs.toStream)
    val dictSmall = Dictionary.fromWordMappings(wmPairs.toStream)
    val quizGroup = QuizGroup(header, quizPairs.toStream, dictionary = dictSmall)

    "be convertible to a QuizGroup" in {
      wmgSmall.toQuizGroup mustEqual quizGroup
    }

    "be constructible from a QuizGroup" in {
      WordMappingGroup.fromQuizGroup(quizGroup) mustEqual wmgSmall
    }
  }
      
}