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
package com.oranda.libanius.util

import org.specs2.mutable.Specification
import com.oranda.libanius.dependencies.AppDependencyAccess

class StringUtilSpec extends Specification {

  "a string should " should {
    "allow a substring to be extracted based on enclosing substrings specified" in {
      val str = "promptType=\"English word\" responseType=\"German word\" mainSeparator=\"|\" currentPromptNumber=\"10\" isActive=\"true\""
      val substr = StringUtil.parseValue(str, "isActive=\"", "\"")
      substr mustEqual Some("true")
    }
  }
}
