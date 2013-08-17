/*
 * Code adapted from the Akka documentation:
 *
 * http://doc.akka.io/docs/akka/2.0.1/intro/getting-started-first-scala.html
 */
package com.oranda.libanius.pi

import akka.actor.Actor
import scala.concurrent.duration.Duration

import com.oranda.libanius.util.Platform

class Listener extends Actor {
  def receive = {
    case PiApproximation(pi, duration) ⇒
      Listener.printPi(pi, duration)
      context.system.shutdown()
  }
}

object Listener extends Platform {
  def printPi(pi: Double, duration: Duration) =
    log("\n\tPi approximation: \t\t%s\n\tCalculation time: \t%s"
      .format(pi, duration))

  def printPi(pi: Double) =
    println("\n\tPi approximation: \t\t%s".format(pi))
}