/*
 * Code adapted from the Akka documentation:
 *
 * http://doc.akka.io/docs/akka/2.0.1/intro/getting-started-first-scala.html
 */
package com.oranda.libanius.pi

import akka.actor._
import com.oranda.libanius.util.Platform


object Pi extends App with Platform {

  // actors and messages ...

  def calculate(nrOfWorkers: Int, nrOfElements: Int, nrOfMessages: Int) {
    val startSystemTime = System.currentTimeMillis

    // Create an Akka system
    val system = ActorSystem("PiSystem")

    log("time to start system: " + (System.currentTimeMillis - startSystemTime))

    // create the result listener, which will print the result and shutdown the system
    val listener = system.actorOf(Props[Listener], name = "listener")

    // create the master
    val master = system.actorOf(Props(new Master(
      nrOfWorkers, nrOfMessages, nrOfElements, listener)),
      name = "master")


    log("time to start system and create actors: " + (System.currentTimeMillis - startSystemTime))


    val startCalcSystemTime = System.currentTimeMillis

    // start the calculation
    master ! Calculate

    log("time to calculate: " + (System.currentTimeMillis - startCalcSystemTime))
  }
}