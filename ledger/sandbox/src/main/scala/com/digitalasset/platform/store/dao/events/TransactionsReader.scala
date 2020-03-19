// Copyright (c) 2020 The DAML Authors. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.digitalasset.platform.store.dao.events

import com.daml.ledger.participant.state.v1.TransactionId
import com.digitalasset.ledger.api.v1.transaction_service.GetFlatTransactionResponse
import com.digitalasset.platform.store.DbType
import com.digitalasset.platform.store.dao.DbDispatcher

import scala.concurrent.{ExecutionContext, Future}

private[dao] object TransactionsReader {

  def apply(
      dispatcher: DbDispatcher,
      dbType: DbType,
      executionContext: ExecutionContext): TransactionsReader =
    new TransactionsReader {
      override def lookupFlatTransactionById(
          transactionId: TransactionId,
          requestingParties: Set[Party],
      ): Future[Option[GetFlatTransactionResponse]] = {
        val query = EventsTable
          .prepareLookupFlatTransactionById(transactionId, requestingParties)
        dispatcher
          .executeSql(
            description = "lookup_flat_transaction_by_id",
            extraLog = Some(s"tx: $transactionId, parties = ${requestingParties.mkString(", ")}"),
          ) { implicit connection =>
            query.as(EventsTable.flatEventParser.*)
          }
          .map(RawFlatEvent.aggregate)(executionContext)
      }
    }
}

private[dao] trait TransactionsReader {

  def lookupFlatTransactionById(
      transactionId: TransactionId,
      requestingParties: Set[Party],
  ): Future[Option[GetFlatTransactionResponse]]

}
