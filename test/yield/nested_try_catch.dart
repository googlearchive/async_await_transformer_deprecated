import 'dart:collection';

void main() {
  print(f()); // prints (1,2,4)
}

f() sync* {
  try {
    try {
      yield 1;
    } finally {
      yield 2;
      return;
    }
    yield 3;
  } finally {
    yield 4;
    return;
  }
}
