import 'dart:async';

main() {
  testInstanceCreation();
}

testInstanceCreation() async {
  print(new List<int>());
  print(new List<int>.from([1, 2, 3]));
  print(new List.from([1, 2, 3]));
  print(new Map<int,String>());
}
