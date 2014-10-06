import 'dart:collection';

void main() {
  print(new String.fromCharCodes(reverse("aibohphobia")));
}

reverse(source) sync* {
  yield* reverse(source.skip(1));
  yield source.first;
}
