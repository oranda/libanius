package com.oranda.libanius
import com.oranda.libanius.model.wordmapping._

object GlobalState {
  
  // Could be lazy val's, but the UI needs some control over when they are initialized.
  var quiz: Option[QuizOfWordMappings] = None

  // a global cache of quiz groups. TODO: use function-local memoization instead!
  var loadedQuizGroups = List[WordMappingGroup]()

  def updateQuiz(newQuiz: QuizOfWordMappings) {
    this.quiz = Some(newQuiz)

    def updateLoadedQuizGroups(quizGroup: WordMappingGroup) {
      loadedQuizGroups = loadedQuizGroups.filter(_.header != quizGroup.header)
      loadedQuizGroups :+= quizGroup
    }

    newQuiz.wordMappingGroups.foreach(updateLoadedQuizGroups(_))
  }
}