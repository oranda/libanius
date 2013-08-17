/*
 * Code adapted from the Akka documentation:
 *
 * http://doc.akka.io/docs/akka/2.0.1/intro/getting-started-first-scala.html
 */
package com.oranda.libanius.pi

import scala.concurrent.duration.Duration

sealed trait PiMessage
case object Calculate extends PiMessage
case class Work(start: Int, nrOfElements: Int) extends PiMessage
case class Result(value: Double) extends PiMessage
case class PiApproximation(pi: Double, duration: Duration)