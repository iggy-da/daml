# Copyright (c) 2019, Digital Asset (Switzerland) GmbH and/or its affiliates.
# All rights reserved.
set -euo pipefail

# Check that DAML compilation is deterministic.
TMP_SRC1=$(mktemp -d)
TMP_SRC2=$(mktemp -d)
TMP_OUT=$(mktemp -d)

cleanup () {
    rm -rf "$TMP_SRC1" "$TMP_SRC2" "$TMP_OUT"
}
trap cleanup EXIT

cp -r daml-foundations/daml-ghc/tests/* "$TMP_SRC1"
cp -r daml-foundations/daml-ghc/tests/* "$TMP_SRC2"

damlc="$PWD/$1"
protoc="$PWD/$2"

(cd "$TMP_SRC1" && $damlc compile "Examples.daml" -o "$TMP_OUT/out_1")
(cd "$TMP_SRC2" && $damlc compile "Examples.daml" -o "$TMP_OUT/out_2")

# When invoked with a project root (as set by the DAML assistant)
# we should produce the same output regardless of the path with which we are invoked.
(cd "/" && DAML_PROJECT="$TMP_SRC1" $damlc compile "$TMP_SRC1/Examples.daml" -o "$TMP_OUT/out_proj_1")
(cd "$TMP_SRC1" && DAML_PROJECT="$TMP_SRC1" $damlc compile "Examples.daml" -o "$TMP_OUT/out_proj_2")

$protoc --decode_raw < "$TMP_OUT/out_1" > "$TMP_OUT/decoded_out_1"
$protoc --decode_raw < "$TMP_OUT/out_2" > "$TMP_OUT/decoded_out_2"
# We first diff the decoded files to get useful debugging output and
# then the non-decoded files to ensure that we actually get bitwise
# identical outputs.
diff -u "$TMP_OUT/decoded_out_1" "$TMP_OUT/decoded_out_2"
diff -u "$TMP_OUT/out_1" "$TMP_OUT/out_2"
$protoc --decode_raw < "$TMP_OUT/out_proj_1" > "$TMP_OUT/decoded_out_proj_1"
$protoc --decode_raw < "$TMP_OUT/out_proj_2" > "$TMP_OUT/decoded_out_proj_2"
diff -u "$TMP_OUT/decoded_out_proj_1" "$TMP_OUT/decoded_out_proj_2"
diff -u "$TMP_OUT/out_proj_1" "$TMP_OUT/out_proj_2"
