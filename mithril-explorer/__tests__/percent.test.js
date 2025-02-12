import { percent } from "@/utils";

describe("Percent computation", () => {
  it("Default number of digit is 0", () => {
    expect(percent(1, 7)).toEqual("14");
  });

  it("Round numbers up", () => {
    expect(percent(2, 3)).toEqual("67");
  });

  it.each([
    [1, 3, 0, "33"],
    [1, 3, 1, "33.3"],
    [1, 3, 2, "33.33"],
    [1, 3, 5, "33.33333"],
  ])("Computing percent", (number, total, number_of_digits, expected) => {
    expect(percent(number, total, number_of_digits)).toEqual(expected);
  });

  it.each([
    [14324, 0],
    [21, 1],
    [423, 5],
  ])("When total is 0 always return 0", (number, number_of_digits) => {
    const total = 0;
    expect(percent(number, total, number_of_digits)).toEqual("0");
  });
});
