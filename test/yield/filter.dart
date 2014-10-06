import 'dart:collection';

void main() {
  print(filter([0, 1, 2, 3, 4, 5], (x) => x.isEven));
}

filter(ss, p) sync* {
  for (var s in ss) {
    if (p(s)) yield s;
  }
}
