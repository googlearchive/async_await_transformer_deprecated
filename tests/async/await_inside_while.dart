import 'dart:async';

main() async {
  await f();
}

f() async {
  var n = 0;
  var lastWrite = new Future.value();
  while(n = await readAsync() > 0) {
    await lastWrite;
    lastWrite = writeAsync(n);
  }
}

var input = 10;

read() async {
  await delay(2000); 
  print(input);
  return input--; 
}

write(n) async {
  print(n);
  await delay(2000);
  return;
}

delay(milliseconds) {
  return new Future.delayed(new Duration(milliseconds: milliseconds));
}
