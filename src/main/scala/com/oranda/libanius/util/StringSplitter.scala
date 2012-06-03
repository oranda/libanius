package com.oranda.libanius.util

import android.text.TextUtils
import java.lang.Character

abstract class StringSplitter(char: Character) extends Iterator[String] {
  override def hasNext: Boolean 
  override def next: String 
  def setString(str: String)
}

class StringSplitterAndroid(_char: Character) extends StringSplitter(_char) {
  
  val splitter = new TextUtils.SimpleStringSplitter(_char)
  
  override def setString(str: String) {
    splitter.setString(str)
  }
  
  override def hasNext: Boolean = splitter.hasNext
  override def next: String = splitter.next
}

class StringSplitterDefault(_char: Character) extends StringSplitter(_char) {

  private var iter: Iterator[String] = _
  
  override def setString(str: String) {
    iter = str.split("\\" +_char.toString).iterator
  }
  
  override def hasNext: Boolean = iter.hasNext
  override def next: String = iter.next  
}