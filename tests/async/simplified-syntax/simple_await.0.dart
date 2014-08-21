main_async() {
  await(f());
}

f_async() {
  var b;
  print(1);
  b = await(g(2));
  print(b);
}

g(n) {
  return new_Future(n);
}
