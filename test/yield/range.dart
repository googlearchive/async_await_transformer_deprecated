import 'dart:collection';

void main() {
  print(range(0, 10));
}

range(start, count) sync* {
  if (count <= 0) return;
  yield start;
  yield* range(start + 1, count - 1);
}