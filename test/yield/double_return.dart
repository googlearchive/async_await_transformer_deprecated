import 'dart:collection';

void main() {
  print(f()); // prints (1,2)
}

f() sync* {
  try {
    yield 1;
    return;
  } finally {
    yield 2;
    return;
  }
}
