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

class WordMappingValueSetSpec extends Specification {

  "a word-mapping-value-set" should {

    val wmvsCustomFormat = "contract|698,696;697/treaty|796;798"

    val wmvs = WordMappingValueSet.fromCustomFormat(wmvsCustomFormat, "|")

    "be parseable from custom format" in {
      wmvs.containsValue("treaty")
      wmvs.size mustEqual 2
      wmvs.toCustomFormat(new StringBuilder(""), "|").toString mustEqual wmvsCustomFormat
    }

    "allow a word-mapping value to be added" in {
      val wmvsUpdated = wmvs.addValueToEnd(WordMappingValue("agreement"))
      wmvsUpdated.size mustEqual 3
    }

    "remove a value" in {
      val wmvsUpdated = wmvs.removeValue("contract")
      wmvsUpdated.containsValue("contract") mustEqual false
    }
  }
}  