Libanius
========

This is the core library for Libanius. The purpose of Libanius is to aid learning. Basically it presents "quiz items" to the user, and for each one the user must select the correct answer option. Quiz items are presented at random according to a certain algorithm based on [spaced repetition](http://en.wikipedia.org/wiki/Spaced_repetition). An item has to be answered correctly several times before it is considered learnt.

The core use is as a vocabulary builder in a new language, but it is designed to be flexible enough to present questions and answers of all types.

The implementation is in Scala. The main target platforms are the Web and Android.

Suggestions for new features and code improvements will be happily received by:

James McCabe <jjtmccabe@gmail.com>


Usage
=====

Usually Libanius will be run through an interface such as https://github.com/oranda/libanius-scalajs-react, but a console UI is provided in this project.
Get a copy of `github.com/oranda/libanius` using `git clone`,  From SBT, type:

    run

Pick the option `com.oranda.libanius.consoleui.RunQuiz` and try out a sample quiz.

This has been tested with Scala 3.1.2, Java 8 (Zulu), and sbt 1.5. For Scala 2.x, try an older 
version: https://github.com/oranda/libanius/archive/refs/tags/v0.9.8.7.3.zip.

A better standalone CLI is available at https://github.com/oranda/libanius-cli-zio.


Making Your Own Quizzes
=======================

If you need to learn a subject, consider making a quiz for it. You need to write a quiz file.
The format is simple. Just study the sample quiz files in the `data/resources` folder. Look at
the first line. You can see that each question is asked a certain number of times depending
on the `numCorrectResponsesRequired` parameter, and multiple-choice is used the first x times,
where x is set using the `useMultipleChoiceUntil`.

The quiz files in `data/resources` have the `.txt` extension. If a user exits a quiz before 
finishing it, the state is saved in a `.qgr` file in the `data` folder. This will be read
on running libanius again. Remember to delete `.qgr` files if you want to start from the 
beginning. 

Whenever the user answers a question correctly, it is said that s/he has moved up a "memory level" 
with respect to that item. The number of memory levels for a quiz is equal to 
`numCorrectResponsesRequired`. 

In any quiz file, quiz items are grouped into "partitions", where each partition corresponds to 
a memory level. You will observe that in the initial quiz file (`.txt`), all quiz items are in 
the first partition. As the user makes progress, quiz items are moved to other partitions, and 
you can observe this effect by quitting a quiz and looking at the persisted form, i.e. the 
`.qgr` file.

If you learn something using your own quiz file, and feel it is a success, consider 
submitting it to this project (e.g. via a PR) so that other people may learn too.


License
=======

Most Libanius source files are made available under the terms of the GNU Affero General Public License (AGPL).
See individual files for details.

Attribution info is in [SOURCES](SOURCES.md).
