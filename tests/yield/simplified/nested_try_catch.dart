// Should yield 1, 2, and 4.
f_syncStar() {
  try {
    try { yield(1); } finally { yield(2); return; }
    yield(3);
  } finally {
    yield(4);
    return;
  }
}
