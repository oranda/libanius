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

import com.oranda.libanius.util.StringUtil
import com.oranda.libanius.util.Platform
import com.oranda.libanius.model.ModelComponent

abstract class WordMappingGroup(val keyType: String, val valueType: String)
    extends ModelComponent {
  // keyType example: "English word"
  // valueType example: "German word"

  def matches(wmgOther: WordMappingGroup): Boolean =
    WordMappingGroup.matches(wmgOther, keyType, valueType)
}

object WordMappingGroup extends Platform {
  // Utility functions for parsing WordMappingGroup's from files
  def splitterLineBreak = getSplitter('\n')
  def splitterKeyValue = getSplitter('|')
  
  def parseKeyType(str: String) = StringUtil.parseValue(str, "keyType=\"", "\"")
  def parseValueType(str: String) = StringUtil.parseValue(str, "valueType=\"", "\"")

  def matches(wmg: WordMappingGroup, keyType: String, valueType: String) =
    keyType == wmg.keyType && valueType == wmg.valueType
}