function checkUrl(url) {
  try {
    // Use the url constructor to check if the value is an url
    return Boolean(new URL(url));
  } catch (ex) {
    return false;
  }
}

module.exports = {
  checkUrl
}