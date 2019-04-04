// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

/*
 * THIS FILE WAS AUTOGENERATED BY THE DIGITAL ASSET DAML SCALA CODE GENERATOR
 * DO NOT EDIT BY HAND!
 */
import _root_.com.digitalasset.ledger.client.{binding => $u0020lfdomainapi}
import _root_.com.digitalasset.ledger.api.v1.{value => $u0020rpcvalue}
package com.digitalasset.sample {
  package Main {
    final case class CallablePayout(
        giver: ` lfdomainapi`.Primitive.Party,
        receiver: ` lfdomainapi`.Primitive.Party)
        extends ` lfdomainapi`.Template[CallablePayout] {
      override protected[this] def templateCompanion(
          implicit ` d`: _root_.scala.Predef.DummyImplicit) = CallablePayout
    }

    object CallablePayout
        extends ` lfdomainapi`.TemplateCompanion[CallablePayout]
        with _root_.scala.Function2[
          ` lfdomainapi`.Primitive.Party,
          ` lfdomainapi`.Primitive.Party,
          _root_.com.digitalasset.sample.Main.CallablePayout] {
      implicit val `CallablePayout Value`
        : ` lfdomainapi`.Value[_root_.com.digitalasset.sample.Main.CallablePayout] = {
        final class $anon
            extends this.`Value ValueRef`[_root_.com.digitalasset.sample.Main.CallablePayout] {

          override def write(
              value: _root_.com.digitalasset.sample.Main.CallablePayout): ` rpcvalue`.Value.Sum =
            ???

          override def read(argValue: ` rpcvalue`.Value.Sum)
            : _root_.scala.Option[_root_.com.digitalasset.sample.Main.CallablePayout] = ???
        };
        new $anon()
      };
      override val id = ` templateId`(
        packageId = "e6a98a45832f0d7060c64f41909c181f2c94bb156232fdde869ca4161cac29a8",
        name = "Main.CallablePayout");
      final implicit class `CallablePayout syntax`(
          private val id: ` lfdomainapi`.Primitive.ContractId[CallablePayout])
          extends _root_.scala.AnyVal {
        def exerciseCall(actor: ` lfdomainapi`.Primitive.Party): ` lfdomainapi`.Primitive.Update[
          ` lfdomainapi`.Primitive.ContractId[com.digitalasset.sample.Main.PayOut]] = ???

        def exerciseTransfer(
            actor: ` lfdomainapi`.Primitive.Party,
            $choice_arg: com.digitalasset.sample.Main.CallablePayout.Transfer)
          : ` lfdomainapi`.Primitive.Update[
            ` lfdomainapi`.Primitive.ContractId[com.digitalasset.sample.Main.CallablePayout]] = ???

      };
      override val consumingChoices: Set[` lfdomainapi`.Primitive.ChoiceId] =
        ` lfdomainapi`.Primitive.ChoiceId.subst(Set("Call", "Transfer"));
      override def toNamedArguments(` self`: CallablePayout) =
        ` arguments`(
          scala.Tuple2("giver", ` lfdomainapi`.Value.encode(` self`.giver)),
          scala.Tuple2("receiver", ` lfdomainapi`.Value.encode(` self`.receiver)));
      final case class Transfer(newReceiver: ` lfdomainapi`.Primitive.Party)
          extends ` lfdomainapi`.ValueRef;
      object Transfer
          extends ` lfdomainapi`.ValueRefCompanion
          with _root_.scala.Function1[
            ` lfdomainapi`.Primitive.Party,
            _root_.com.digitalasset.sample.Main.CallablePayout.Transfer] {
        implicit val `Transfer Value`
          : ` lfdomainapi`.Value[_root_.com.digitalasset.sample.Main.CallablePayout.Transfer] = {
          final class $anon
              extends this.`Value ValueRef`[
                _root_.com.digitalasset.sample.Main.CallablePayout.Transfer] {
            override def write(value: _root_.com.digitalasset.sample.Main.CallablePayout.Transfer)
              : ` rpcvalue`.Value.Sum =
              ` record`(
                scala.Tuple2("newReceiver", ` lfdomainapi`.Value.encode(value.newReceiver)));
            override def read(argValue: ` rpcvalue`.Value.Sum)
              : _root_.scala.Option[_root_.com.digitalasset.sample.Main.CallablePayout.Transfer] =
              argValue.record.flatMap(((` r`) =>
                ` r`.fields match {
                  case _root_.scala.collection.Seq(
                      ` rpcvalue`.RecordField(("" | "newReceiver"), _root_.scala.Some(zv0))) =>
                    ` lfdomainapi`.Value
                      .decode[` lfdomainapi`.Primitive.Party](zv0)
                      .map(((newReceiver) => Transfer(newReceiver)))
                  case _ => _root_.scala.None
                }))
          };
          new $anon()
        };
        override protected val ` recordOrVariantId` = _root_.scala.Some(
          ` rpcvalue`.Identifier(
            "e6a98a45832f0d7060c64f41909c181f2c94bb156232fdde869ca4161cac29a8",
            "Main.CallablePayout.Transfer"))
      }
    }
  }
}
