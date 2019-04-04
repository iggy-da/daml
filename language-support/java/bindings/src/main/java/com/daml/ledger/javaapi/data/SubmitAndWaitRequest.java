// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.ledger.javaapi.data;

import com.digitalasset.ledger.api.v1.CommandServiceOuterClass;
import org.checkerframework.checker.nullness.qual.NonNull;

import java.time.Instant;
import java.util.List;

public class SubmitAndWaitRequest {

    public static CommandServiceOuterClass.SubmitAndWaitRequest toProto(@NonNull String ledgerId,
                                                                                                     @NonNull String workflowId,
                                                                                                     @NonNull String applicationId,
                                                                                                     @NonNull String commandId,
                                                                                                     @NonNull String party,
                                                                                                     @NonNull Instant ledgerEffectiveTime,
                                                                                                     @NonNull Instant maximumRecordTime,
                                                                                                     @NonNull List<@NonNull Command> commands) {
        return CommandServiceOuterClass.SubmitAndWaitRequest.newBuilder()
                .setCommands(SubmitCommandsRequest.toProto(ledgerId, workflowId, applicationId,
                        commandId, party, ledgerEffectiveTime, maximumRecordTime, commands))
                .build();
    }
}
