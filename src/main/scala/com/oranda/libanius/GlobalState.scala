package com.oranda.libanius
import com.oranda.libanius.model.wordmapping._

object GlobalState {

  var quiz: QuizOfWordMappings = QuizOfWordMappings()

  // a global cache of quiz groups. TODO: use function-local memoization instead!
  var loadedQuizGroups = List[WordMappingGroup]()

  def updateQuiz(newQuiz: QuizOfWordMappings) {
    this.quiz = newQuiz

    def updateLoadedQuizGroups(quizGroup: WordMappingGroup) {
      loadedQuizGroups = loadedQuizGroups.filter(_.header != quizGroup.header)
      loadedQuizGroups :+= quizGroup
    }

    newQuiz.wordMappingGroups.foreach(updateLoadedQuizGroups(_))
  }

  def numActiveQuizGroups = quiz.wordMappingGroups.size
}