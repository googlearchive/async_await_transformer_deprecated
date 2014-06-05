import 'dart:collection';

void main() {
   print(f()); 
}

f() sync* {
  var PC = 0; var i = 2;
  switch(PC) {
     _0: case 0:
        yield 1;
        continue _1;
     break;
     _1: case 1:
        if(i— <= 0) return;
        yield 2;
        continue _0;
     break;
  }
  yield 3;
}