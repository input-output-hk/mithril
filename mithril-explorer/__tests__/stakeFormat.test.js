import {formatStake} from "../src/utils";

const toLovelace = (ada) => ada * 1000000;

describe('Stake formatting', () => {
  it('formatting a string returns NaN', () => {
    expect(formatStake("string")).toEqual("NaN₳");
  });
  
  it.each([
    [0.13214, "0.13₳"],
    [0.9182, "0.92₳"],
    [10.1398, "10.14₳"],
    [923.170, "923.17₳"],
  ])('formatting Ada limit fraction to digits to 2 with rounding', (ada, expected) => {
    expect(formatStake(toLovelace(ada))).toEqual(expected);
  });
  
  it.each([
    [100, "100₳"],
    [289.239, "289.24₳"],
    [583.999, "584₳"],
    [999.99, "999.99₳"],
  ])('formatting less than 1K Ada does not add power suffix', (ada, expected) => {
    expect(formatStake(toLovelace(ada))).toEqual(expected);
  });
  
  it.each([
    [1000, "1K₳"],
    [23289.239, "23.29K₳"],
    [583999.999, "584K₳"],
    [999990, "999.99K₳"],
  ])('formatting between 1K and 1M Ada add the `K` suffix', (ada, expected) => {
    expect(formatStake(toLovelace(ada))).toEqual(expected);
  });
  
  it.each([
    [1000000, "1M₳"],
    [23289000.239, "23.29M₳"],
    [583999000.999, "584M₳"],
    [999990000, "999.99M₳"],
  ])('formatting between 1M and 1B Ada add the `M` suffix', (ada, expected) => {
    expect(formatStake(toLovelace(ada))).toEqual(expected);
  });
  
  it.each([
    [1000000000, "1B₳"],
    [23289000000.239, "23.29B₳"],
    [583999000000.999, "584B₳"],
    [9999990000000, "9,999.99B₳"],
  ])('formatting more than 1B Ada add the `B` suffix', (ada, expected) => {
    expect(formatStake(toLovelace(ada))).toEqual(expected);
  });

  it.each([
    [999.999, "1K₳"],
    [999999.999, "1M₳"],
    [999999999.999, "1B₳"],
  ])('Handle rounding up to next power suffix', (ada, expected) => {
    expect(formatStake(toLovelace(ada))).toEqual(expected);
  });
});
