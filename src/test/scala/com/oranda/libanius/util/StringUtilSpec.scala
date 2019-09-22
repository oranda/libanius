/*
 * Libanius
 * Copyright (C) 2012-2019 James McCabe <james@oranda.com>
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

package com.oranda.libanius.util

import org.specs2.mutable.Specification

class StringUtilSpec extends Specification {

  "a string should " should {
    "allow a substring to be extracted based on enclosing substrings specified" in {
      val str = """promptType="English word" responseType="German word" mainSeparator="|" currentPromptNumber="10" isActive="true""""
      val substr = StringUtil.parseValue(str, "isActive=\"", "\"")
      substr mustEqual Some("true")
    }
  }
}
