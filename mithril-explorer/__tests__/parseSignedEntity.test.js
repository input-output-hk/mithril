import { parseSignedEntity } from "@/utils";
import { signedEntityType } from "@/constants";

describe("parseSignedEntity", () => {
  it("parse correctly formed `MithrilStakeDistribution` signed entity", () => {
    const epoch = 1432;
    const signed_entity = {
      [signedEntityType.MithrilStakeDistribution]: [epoch],
    };

    expect(parseSignedEntity(signed_entity)).toEqual({
      name: signedEntityType.MithrilStakeDistribution,
      fields: {
        epoch: epoch,
      },
    });
  });

  it("parse correctly formed `CardanoStakeDistribution` signed entity", () => {
    const epoch = 1432;
    const signed_entity = {
      [signedEntityType.CardanoStakeDistribution]: [epoch],
    };

    expect(parseSignedEntity(signed_entity)).toEqual({
      name: signedEntityType.CardanoStakeDistribution,
      fields: {
        epoch: epoch,
      },
    });
  });

  it("parse correctly formed `CardanoImmutableFilesFull` signed entity", () => {
    const [epoch, immutable_file_number] = [1432, 7548];
    const signed_entity = {
      [signedEntityType.CardanoImmutableFilesFull]: {
        epoch: epoch,
        immutable_file_number: immutable_file_number,
      },
    };

    expect(parseSignedEntity(signed_entity)).toEqual({
      name: signedEntityType.CardanoImmutableFilesFull,
      fields: {
        epoch: epoch,
        immutable_file_number: immutable_file_number,
      },
    });
  });

  it("parse correctly formed `CardanoDatabase` signed entity", () => {
    const [epoch, immutable_file_number] = [1432, 7548];
    const signed_entity = {
      [signedEntityType.CardanoDb]: {
        epoch: epoch,
        immutable_file_number: immutable_file_number,
      },
    };

    expect(parseSignedEntity(signed_entity)).toEqual({
      name: signedEntityType.CardanoDb,
      fields: {
        epoch: epoch,
        immutable_file_number: immutable_file_number,
      },
    });
  });

  it("parse correctly formed `CardanoTransactions` signed entity", () => {
    const [epoch, block_number] = [1432, 23891];
    const signed_entity = {
      [signedEntityType.CardanoTransactions]: [epoch, block_number],
    };

    expect(parseSignedEntity(signed_entity)).toEqual({
      name: signedEntityType.CardanoTransactions,
      fields: {
        epoch: epoch,
        block_number: block_number,
      },
    });
  });

  it("parse correctly formed `CardanoBlocksTransactions` signed entity", () => {
    const [epoch, block_number, security_parameter] = [1432, 23891, 1000];
    const signed_entity = {
      [signedEntityType.CardanoBlocksTransactions]: [epoch, block_number, security_parameter],
    };

    expect(parseSignedEntity(signed_entity)).toEqual({
      name: signedEntityType.CardanoBlocksTransactions,
      fields: {
        epoch: epoch,
        block_number: block_number,
        security_parameter: security_parameter,
      },
    });
  });

  it("parse unknown array value", () => {
    const [foo, bar] = ["foo", "bar"];
    const signed_entity = {
      unknown: [foo, bar],
    };

    expect(parseSignedEntity(signed_entity)).toEqual({
      name: "unknown",
      fields: [foo, bar],
    });
  });

  it("parse unknown value with a simple object", () => {
    const foo = "foo";
    const signed_entity = {
      unknown: {
        foo: foo,
      },
    };

    expect(parseSignedEntity(signed_entity)).toEqual({
      name: "unknown",
      fields: {
        foo: foo,
      },
    });
  });

  it("parse unknown value with complex object", () => {
    const signed_entity = {
      bar: {
        obj: { val: "bar" },
        array: [1, 2, 3],
      },
      foo: "foo",
    };

    expect(parseSignedEntity(signed_entity)).toEqual({
      name: "bar",
      fields: {
        obj: { val: "bar" },
        array: [1, 2, 3],
      },
    });
  });
});
