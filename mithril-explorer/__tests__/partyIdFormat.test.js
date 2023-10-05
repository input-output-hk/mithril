import {formatPartyId} from "../src/utils";

describe('Stake formatting', () => {
  it.each([
    ["pool1zmtm8yef33z2n7x4nn0kvv9xpzjuj7725p9y9m5t960g5qy51ua", "pool1zmtm8…y51ua"],
    ["pool23kk0fksdayg23htnj372avmnwwql9c2zz0ah8jt63rjdzyjr95n", "pool23kk0f…jr95n"],
  ])('formatting party id remove all but the 10 first and the last 5 characters', (partyId, expected) => {
    expect(formatPartyId(partyId)).toEqual(expected);
  });

  it.each([
    ["pool1zmtm8yef33zuj7725p9y9m5t960g5qy51ua", "pool1zmtm8…y51ua"],
    ["pool1zmtm8yef33z22n7x4nn0kvv9xpzjn7x4nn0kvv9xpzjuj7725p9y9m5t960g5qy51ua", "pool1zmtm8…y51ua"],
    ["pool23kk0fyjr95n", "pool23kk0f…jr95n"],
    ["pool23kk0fjr95n", "pool23kk0fjr95n"],
    ["pool23kk0jr95n", "pool23kk0jr95n"],
    ["pool25n", "pool25n"],
  ])('formatting party id remove all but the 10 first and the last 5 characters even for non usual length', (partyId, expected) => {
    expect(formatPartyId(partyId)).toEqual(expected);
  });
});
