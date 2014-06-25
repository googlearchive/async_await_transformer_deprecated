main_async() {
  var error, a, e;
  error = 0;
  try {
    try {
      a = await(f());
    } catch (e) {
      error = e;
    }
  } finally {
    if (not_equal(error, 0)) {
      print(error);
    } else {
    }
    if (not_equal(a, 0)) {
      print(a);
    } else {
    }
  }
}

f_async() {
  try {
    await(g(0));
    return 42;
  } finally {
    await(h(1));
    return 4711;
  }
}

g_async(n) { 
  print(n); 
  return n; 
}

h_async(n) { 
  print(n); 
  throw n; 
}
