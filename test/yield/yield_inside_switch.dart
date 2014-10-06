import 'dart:collection';

void main() {
  print(f());
}

f() sync* {
  var pc = 0;
  var i = 2;
  switch (pc) {
    L0: case 0:
      yield 1;
      continue L1;
    L1: case 1:
      if (i-- <= 0) return;
      yield 2;
      continue L0;
  }
  yield 3;
}
