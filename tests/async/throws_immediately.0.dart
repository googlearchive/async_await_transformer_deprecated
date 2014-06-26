main_async() {
  var passed, a, e;
  passed = 0;
  try {
    a = f();
    passed = 1;
    await(a);
  } catch (e) {
    if (passed) {
    } else {
      print(e);
    }
  }
}

f() {
  throw 0;
}
