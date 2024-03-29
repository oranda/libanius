/*
 * Libanius
 * Copyright (C) 2012-2022 James McCabe <jjtmccabe@gmail.com>
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

package com.oranda.libanius.consoleui

object ConsoleUtil {

  /*
   * Predef readLine does not allow backspacing. This is a hack until libanius SBT is
   * upgraded to a version (0.13 or greater) that is not incompatible with jline 2.11.
   */
  def readLineUntilNoBackspaces: String = {
    val s                               = scala.io.StdIn.readLine
    val extendedCode: (Char) => Boolean = (c: Char) => (c == 127)
    if !s.exists(extendedCode) then s
    else {
      println("backspace detected: try again")
      readLineUntilNoBackspaces
    }
  }
}
