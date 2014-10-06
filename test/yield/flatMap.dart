import 'dart:collection';

void main() {
  print(flatMap([0, 1, 2, 3, 4, 5], (x) => [x, x]));
}

flatMap(ss, f) sync* {
  for (var s in ss) yield* f(s);
}