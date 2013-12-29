/*
 * Libanius
 * Copyright (C) 2012-2014 James McCabe <james@oranda.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.oranda.libanius.model.wordmapping

import org.specs2.mutable.Specification
import com.oranda.libanius.dependencies.{AppDependencyAccess}
import java.lang.StringBuilder

class WordMappingValueSpec extends Specification with AppDependencyAccess {
  
  "a word-mapping value" should {

    val wmvCustomFormat = "nachlösen|9,7;6"

    val wmv = WordMappingValue.fromCustomFormat(wmvCustomFormat, "|")
    
    "be parseable from custom format" in {
      wmv.value mustEqual "nachlösen"
      wmv.userAnswers.length mustEqual 3
      wmv.numCorrectAnswersInARow mustEqual 2
      wmv.toCustomFormat(new StringBuilder(), "|").toString mustEqual wmvCustomFormat
    }
  }
  
}