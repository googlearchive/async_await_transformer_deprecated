main_async() {
  await(f());
}

f_async() {
  var n, lastWrite;
  n = 0;
  lastWrite = new_Future_value();
L0: while(n = greater(await(readAsync()), 0)) {
    await(lastWrite);
    lastWrite = writeAsync(n);
  }
}

read_async() {
  var read;
  read = 10;
  await(delay(2000)); 
  print(read);
  return decrement(read); 
}

write_async(n) {
  print(n);
  await(delay(2000));
  return 0;
}

delay(milliseconds) {
   return new_Future_delayed(new_Duration(milliseconds));
}
