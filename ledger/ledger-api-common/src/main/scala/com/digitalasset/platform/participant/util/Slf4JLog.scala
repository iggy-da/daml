// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.digitalasset.platform.participant.util

import akka.stream._
import akka.stream.stage.{GraphStage, GraphStageLogic, InHandler, OutHandler}
import org.slf4j.Logger

final case class Slf4JLog[T, U](
    logger: Logger,
    prefix: String,
    project: T => U,
    logDemand: Boolean = false)
    extends GraphStage[FlowShape[T, T]] {

  override def toString = "Slf4JLog"

  val in: Inlet[T] = Inlet[T]("in")
  val out: Outlet[T] = Outlet[T]("out")

  override def shape: FlowShape[T, T] = FlowShape(in, out)

  override def createLogic(inheritedAttributes: Attributes): GraphStageLogic =
    new GraphStageLogic(shape) with OutHandler with InHandler {

      override def onPush(): Unit = {

        val elem = grab(in)
        if (logger.isDebugEnabled) logger.debug("[{}] Element: {}", prefix, project(elem))
        push(out, elem)
      }

      override def onPull(): Unit = {
        if (logDemand) logger.debug("[{}] Demand", prefix)
        pull(in)
      }

      override def onUpstreamFailure(cause: Throwable): Unit = {
        logger.warn(s"[$prefix] Upstream failed", cause)

        super.onUpstreamFailure(cause)
      }

      override def onUpstreamFinish(): Unit = {
        logger.debug("[{}] Upstream finished.", prefix)

        super.onUpstreamFinish()
      }

      override def onDownstreamFinish(): Unit = {
        logger.debug("[{}] Downstream finished.", prefix)

        super.onDownstreamFinish()
      }

      setHandlers(in, out, this)
    }
}

object Slf4JLog {
  def apply[T](logger: Logger, prefix: String): Slf4JLog[T, T] =
    new Slf4JLog(logger, prefix, identity)
  def apply[T](logger: Logger, prefix: String, logDemand: Boolean): Slf4JLog[T, T] =
    new Slf4JLog(logger, prefix, identity, logDemand)
}
