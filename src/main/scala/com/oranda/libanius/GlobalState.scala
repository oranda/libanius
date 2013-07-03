package com.oranda.libanius
import com.oranda.libanius.model.wordmapping.WordMappingGroupReadOnly
import com.oranda.libanius.model.wordmapping.QuizOfWordMappings

object GlobalState {
  
  // Could be lazy val's, but the UI needs some control over when they are initialized.
  var quiz: Option[QuizOfWordMappings] = None
  var dictionary: Option[WordMappingGroupReadOnly] = None
  
  def initQuiz(fn: => QuizOfWordMappings) {
    if (!GlobalState.quiz.isDefined)
      this.quiz = Some(fn)
  }
  
  def initDictionary(fn: => WordMappingGroupReadOnly) {
    if (!GlobalState.dictionary.isDefined)
      this.dictionary = Some(fn)
  } 
}