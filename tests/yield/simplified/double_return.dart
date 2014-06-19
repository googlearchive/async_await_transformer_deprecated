// Should yield both 1 and 2.
f_syncStar() {
  try {
    yield(1);
    return;
  } finally {
    yield(2);
    return;
  }
}
