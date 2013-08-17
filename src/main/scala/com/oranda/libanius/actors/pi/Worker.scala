/*
 * Code adapted from the Akka documentation:
 *
 * http://doc.akka.io/docs/akka/2.0.1/intro/getting-started-first-scala.html
 */
package com.oranda.libanius.pi

import akka.actor._

class Worker extends Actor {

  // calculatePiFor ...

  def receive = {
    case Work(start, nrOfElements) ⇒
      sender ! Result(Worker.calculatePiFor(start, nrOfElements)) // perform the work
  }

}

object Worker {

  def calculatePiFor(start: Int, nrOfElements: Int): Double = {
    var acc = 0.0
    for (i ← start until (start + nrOfElements))
      acc += 4.0 * (1 - (i % 2) * 2) / (2 * i + 1)
    acc
  }
}
