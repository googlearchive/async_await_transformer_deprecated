main_async() {
  var a;
  a = await(f());
  print(a);
}

f_asyc() {
  return g();
}
  
g_async() {
  return 42;
}
