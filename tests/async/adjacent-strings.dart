import 'dart:async';

comma() async => ',';
space() async => ' ';

main() async {
  print('Hello' '${await comma()}${await space()}' 'World!');
}
