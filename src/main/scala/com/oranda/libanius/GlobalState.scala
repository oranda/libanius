package com.oranda.libanius
import com.oranda.libanius.model.wordmapping.WordMappingGroupReadOnly
import com.oranda.libanius.model.wordmapping.QuizOfWordMappings

object GlobalState {
  
  var quiz: Option[QuizOfWordMappings] = None
  var dictionary: Option[WordMappingGroupReadOnly] = None
}