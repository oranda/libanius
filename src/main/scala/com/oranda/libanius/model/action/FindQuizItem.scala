package com.oranda.libanius.model.action

import com.oranda.libanius.model.Quiz
import com.oranda.libanius.model.action.QuizItemSource.{findAnyUnfinishedQuizItem, produceQuizItem}
import com.oranda.libanius.model.quizitem.QuizItemViewWithChoices
import modelComponentsAsQuizItemSources._

object FindQuizItem {
  def run(quiz: Quiz): Option[QuizItemViewWithChoices] =
    produceQuizItem(quiz, NoParams()).orElse(findAnyUnfinishedQuizItem(quiz, NoParams()))
}
