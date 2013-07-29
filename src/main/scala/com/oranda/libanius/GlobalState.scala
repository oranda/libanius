package com.oranda.libanius
import com.oranda.libanius.model.wordmapping._

object GlobalState {
  
  // Could be lazy val's, but the UI needs some control over when they are initialized.
  var quiz: Option[QuizOfWordMappings] = None
  var dictionary: Option[WordMappingGroupReadOnly] = None

  // a global cache of quiz groups. TODO: use function-local memoization instead!
  var loadedQuizGroups = List[WordMappingGroupReadWrite]()

  def updateQuiz(newQuiz: QuizOfWordMappings) {
    this.quiz = Some(newQuiz)

    def updateLoadedQuizGroups(quizGroup: WordMappingGroupReadWrite) {
      loadedQuizGroups = loadedQuizGroups.filter(_.header != quizGroup.header)
      loadedQuizGroups :+= quizGroup
    }

    newQuiz.wordMappingGroups.foreach(updateLoadedQuizGroups(_))
  }

  def initDictionary(fn: => WordMappingGroupReadOnly) {
    if (!GlobalState.dictionary.isDefined)
      this.dictionary = Some(fn)
  } 
}