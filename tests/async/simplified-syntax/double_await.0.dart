main_async() {
  await(await(f()));
}

f_async() {
  print(0);
}
