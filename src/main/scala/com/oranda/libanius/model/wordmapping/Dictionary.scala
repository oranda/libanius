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

import scala.collection.JavaConversions.mapAsScalaMap

import com.oranda.libanius.util.Platform
import scala.collection.immutable.Stream
import scala.util.Try

/**
 * A dictionary. A large read-only repository of word mappings, structured as a map
 * for fast access.
 */
case class Dictionary() extends Platform {

  // When populating, the java.util map is faster than the mutable Scala map
  private val wordMappings = new java.util.LinkedHashMap[String, WordMappingValueSetWrapperBase]

  def numKeyWords = wordMappings.size
  
  def mappingsForKeysBeginningWith(keyStart: String):
      List[(String, WordMappingValueSetWrapperBase)] = {
    val matchingKeys = wordMappings.keys.filter(_.toLowerCase.startsWith(keyStart.toLowerCase))
    pairListForKeys(matchingKeys, 5)
  }  
   
  def mappingsForKeysContaining(keyPart: String):
      List[(String, WordMappingValueSetWrapperBase)] = {
    val matchingKeys = wordMappings.keys.filter(_.toLowerCase.contains(keyPart.toLowerCase))
    pairListForKeys(matchingKeys, 5)
  }
  
  def pairListForKeys(keys: Iterable[String], size: Int):
      List[(String, WordMappingValueSetWrapperBase)] = {
    val keysSubset = keys.toList.sorted.slice(0, size)
    keysSubset.map(key => (key, wordMappings.get(key)))
  }
  
  def findValuesFor(keyWord: String): WordMappingValueSetWrapperBase = wordMappings.get(keyWord)
}


object Dictionary {

  def fromWordMappings(wordMappingsStream: Stream[WordMappingPair]) =
    new Dictionary() {
      wordMappingsStream.foreach(pair => wordMappings.put(pair.key, pair.valueSet))
    }


  /*
   * Example:
   *
   * wordMappingGroup keyType="English word" valueType="German word"
   *    against|wider
   *    entertain|unterhalten
   */
  def fromCustomFormat(str: String): Dictionary =

    new Dictionary() {

      def parseCustomFormat = {
        val splitterLineBreak = WordMappingGroup.splitterLineBreak
        val splitterKeyValue = WordMappingGroup.splitterKeyValue
        splitterLineBreak.setString(str)
        splitterLineBreak.next // skip the first line, which has already been parsed

        while (splitterLineBreak.hasNext) {
          splitterKeyValue.setString(splitterLineBreak.next)

          if (splitterKeyValue.hasNext) {
            val strKey = splitterKeyValue.next
            if (splitterKeyValue.hasNext) {
              val strValues = splitterKeyValue.next
              // for efficiency, avoid an extra method call into Dictionary here
              wordMappings.put(strKey, WordMappingValueSetLazyProxy(strValues))
            }
          }
        }
      }

      Try(parseCustomFormat) recover {
        case e: Exception => logError("Could not parse dictionary: " + e.getMessage(), e)
        None
      }
    }
}