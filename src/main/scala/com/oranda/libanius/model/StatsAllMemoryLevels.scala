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

package com.oranda.libanius.model

import scalaz.Lens
import scala.collection.immutable.{Map, ListMap}
import scalaz.PLens._

case class StatsAllMemoryLevels(stats: Map[Int, StatsMemoryLevel] = ListMap()) {

  def incrementResponsesCorrect(memoryLevel: Int) = incrementResponses(memoryLevel, true)
  def incrementResponsesIncorrect(memoryLevel: Int) = incrementResponses(memoryLevel, false)

  def incrementResponses(memoryLevel: Int, isCorrect: Boolean) = {
    val statsWithNewLevel =
      if (stats.isDefinedAt(memoryLevel)) stats
      else stats + (memoryLevel -> StatsMemoryLevel())
    val statsWithNewLevelHolder = StatsAllMemoryLevels(statsWithNewLevel)
    StatsAllMemoryLevels.memoryLevelsLens.set(statsWithNewLevelHolder,
        mapVPLens(memoryLevel) mod ((_: StatsMemoryLevel).inc(isCorrect), statsWithNewLevel))
  }

  def totalResponses(level: Int): Int =
    stats.get(level).map(_.totalResponses).getOrElse(0)

  def numCorrectResponsesInARow(level: Int): Int =
    stats.get(level).map(_.numCorrectResponsesInARow).getOrElse(0)

  def reportIfAtLimit(level: Int): Option[String] =
    for (statsForLevel <- stats.get(level) if (statsForLevel.isAtLimit))
    yield level + ": " + statsForLevel

  def reportIfAtLimit: String = {
    stats.collect {
      case (level, statsForLevel) if (statsForLevel.isAtLimit) => level + ": " + statsForLevel
    }.mkString("\n")
  }

  override def toString = stats.toString
}

object StatsAllMemoryLevels {

  val memoryLevelsLens: Lens[StatsAllMemoryLevels, Map[Int, StatsMemoryLevel]] = Lens.lensu(
    get = (_: StatsAllMemoryLevels).stats,
    set = (statsAll: StatsAllMemoryLevels, s: Map[Int, StatsMemoryLevel]) =>
        statsAll.copy(stats = s))
}

case class StatsMemoryLevel(totalResponses: Int = 0, numCorrectResponsesInARow: Int = 0) {

  def isAtLimit = totalResponses >= StatsMemoryLevel.totalResponsesLimit

  def inc(isCorrect: Boolean) = {
    val numCorrectToAdd = if (isCorrect) 1 else 0
    // For each memory level, only check the recent performance. Reset the counters after a limit.
    if (!isAtLimit)
      StatsMemoryLevel(totalResponses + 1, numCorrectResponsesInARow + numCorrectToAdd)
    else // reset the counters
      StatsMemoryLevel(1, numCorrectToAdd)
  }

  override def toString = numCorrectResponsesInARow + "/" + totalResponses
}

object StatsMemoryLevel {
  val totalResponsesLimit = 10
}

