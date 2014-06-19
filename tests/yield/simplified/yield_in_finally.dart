f_syncStar() {
  try {
    try {
      yield(1);
      throw 2;
    } catch (e) {
      yield(3);
      throw 4;
    }
  } finally {
    yield(5);
  }
}
