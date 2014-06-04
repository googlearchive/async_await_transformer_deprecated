import 'dart:async';

main async {
   await f();
}

f() async {
 var n = 0;
 var lastWrite = new Future.value();
 while(n = await readAsync() > 0) {
   await lastWrite;
   lastWrite = writeAsync(n);
}

var read = 10;

readAsync() {
    await delay(2000); 
    print(read);
    return read--; 
}

writeAsync(n) {
  print(n);
  await delay(2000);
  return ;
}

delay(milliseconds) {
   return new Future.delayed(new Duration(milliseconds: milliseconds));
}