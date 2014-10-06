import 'dart:collection';

void main() {
  print(concat([0, 1, 2, 3, 4], [5, 6, 7, 8, 9]));
}

concat(left, right) sync* {
  yield* left;
  yield* right;
}
