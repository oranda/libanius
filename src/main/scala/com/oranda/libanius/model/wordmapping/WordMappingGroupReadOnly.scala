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

package com.oranda.libanius.model.wordmapping

import scala.collection.JavaConversions.mapAsScalaMap
import scala.collection.immutable.HashSet
import scala.collection.mutable
import scala.util.Random
import android.util.Log
import com.oranda.libanius.util.StringUtil
import com.sun.xml.internal.ws.util.StringUtils
import android.text.TextUtils
import com.oranda.libanius.util.Util
import com.oranda.libanius.util.Platform

import com.oranda.libanius.model.ModelComponent

/**
 * A dictionary. A large read-only repository of word mappings, which can be 
 * copied and used by more dynamic parts of the application.
 * 
 * The raison d'etre is that it can be loaded quickly: the values for each
 * key value are only parsed on demand.
 */
case class WordMappingGroupReadOnly(_keyType: String, _valueType: String) 
    extends WordMappingGroup(_keyType, _valueType) with Platform {
  
  // When populating, the java.util map is faster than the mutable Scala map
  private val wordMappings = new java.util.LinkedHashMap[String, String]
  
  // ... but apart from that we want a Scala version of the map so as to use Scala syntax
  def wordMappingsAsScala: mutable.Map[String, String] = wordMappings
  
  def numKeyWords = wordMappings.size
  
  def mappingsForKeysBeginningWith(keyStart: String): List[(String, String)] = {
    val matchingKeys = wordMappings.keys.filter(
        _.toLowerCase.startsWith(keyStart.toLowerCase))
    pairListForKeys(matchingKeys, 5)
  }  
   
  def mappingsForKeysContaining(keyPart: String): List[(String, String)] = {
    val matchingKeys = wordMappings.keys.filter(
        _.toLowerCase.contains(keyPart.toLowerCase))
    pairListForKeys(matchingKeys, 5)
  }
  
  def pairListForKeys(keys: Iterable[String], size: Int): List[(String, String)] = {
    val keysSubset = keys.toList.sorted.slice(0, size)
    keysSubset.map(key => (key, wordMappings.get(key)))    
  }
  
  def findValuesFor(keyWord: String): String = wordMappings.get(keyWord)
}


object WordMappingGroupReadOnly {
      
  val splitterLineBreak = WordMappingGroup.splitterLineBreak
  val splitterKeyValue = WordMappingGroup.splitterKeyValue
  
  /*
   * Example:
   * 
   * wordMappingGroup keyType="English word" valueType="German word"
   *    against|wider
   *    entertain|unterhalten
   */
  def fromCustomFormat(str: String): WordMappingGroupReadOnly =
    new WordMappingGroupReadOnly(_keyType = WordMappingGroup.parseKeyType(str),
        _valueType = WordMappingGroup.parseValueType(str)) {
      
      splitterLineBreak.setString(str)
      splitterLineBreak.next // skip the first line, which has already been parsed
      
      while (splitterLineBreak.hasNext) {
        splitterKeyValue.setString(splitterLineBreak.next)
        
        if (splitterKeyValue.hasNext) {
          val strKey = splitterKeyValue.next
          if (splitterKeyValue.hasNext) {
            val strValues = splitterKeyValue.next
            // for efficiency, avoid an extra method call into Dictionary here
            wordMappings.put(strKey, strValues)
          }
        }
      }
    }
}