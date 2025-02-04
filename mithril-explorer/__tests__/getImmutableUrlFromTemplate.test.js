import { getImmutableUrlFromTemplate } from "@/utils";

const urlTemplate = "https://example.com/{immutable_file_number}.tar.zst";

describe("Get Immutable Url from template", () => {
  it.each([
    [0, "https://example.com/00000.tar.zst"],
    [1, "https://example.com/00001.tar.zst"],
    [23, "https://example.com/00023.tar.zst"],
    [437, "https://example.com/00437.tar.zst"],
    [9428, "https://example.com/09428.tar.zst"],
    [52983, "https://example.com/52983.tar.zst"],
    [103202, "https://example.com/103202.tar.zst"],
  ])("padding immutable file number with zeros to 5 digits", (immutableFileNumber, expected) => {
    expect(getImmutableUrlFromTemplate(urlTemplate, immutableFileNumber)).toEqual(expected);
  });

  it("numbers with more than 5 digits are not cut", () => {
    expect(getImmutableUrlFromTemplate(urlTemplate, 103202)).toEqual(
      "https://example.com/103202.tar.zst",
    );
  });
});
