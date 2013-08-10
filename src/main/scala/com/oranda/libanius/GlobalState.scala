package com.oranda.libanius
import com.oranda.libanius.model.wordmapping._
import com.oranda.libanius.util.Platform

object GlobalState extends Platform {

  // The quiz needs to be global because it is shared between the OptionsScreen and the QuizScreen.
  var quiz: QuizOfWordMappings = QuizOfWordMappings()

  def numActiveQuizGroups = quiz.wordMappingGroups.size

  // A cache of quiz groups. It needs to be global because it must be updated when the quiz is.
  // TODO: use function-local memoization instead!
  var loadedQuizGroups = List[WordMappingGroup]()

  def updateQuiz(newQuiz: QuizOfWordMappings) {
    GlobalState.quiz = newQuiz
    newQuiz.wordMappingGroups.foreach(updateLoadedQuizGroups(_))
  }

  def updateLoadedQuizGroups(quizGroup: WordMappingGroup) {
    loadedQuizGroups = loadedQuizGroups.filter(_.header != quizGroup.header)
    loadedQuizGroups :+= quizGroup
  }
}