main_async() {
  await(f());
}

f_async() {
  var a, b;
  a = await(g(42));
  b = await(g(4711));
  return add(a, b);
}

g_async(n) {
   print(n); 
   return n; 
}
