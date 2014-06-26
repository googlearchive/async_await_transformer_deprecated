main_async() {
  var a, e;
  a = f();
  try {
    await(a);
  } catch (e) {
    print(e);
  }
}

f_async() {
  try {
    try {
      await(g(1));
    } catch (e) {
      await(g(2));
    }
  } finally {
    await(g(3));
  }
}

g_async(n) { 
  print(n); 
  throw n;
}
