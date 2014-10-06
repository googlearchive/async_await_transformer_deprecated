import 'dart:async';

main() async {
  await awaitany(constant(1), constant(2));
}

constant(x) async => x;

awaitany(a,b) async {
  var completer = new Completer();
  try {
    a.then((x){
      try {
        completer.complete(x);
      } catch (e) {
      }
    });
    b.then((x){
      try {
        completer.complete(x);
      } catch (e) {
      }
    });
  } finally { 
    return await completer.future;
  }
}
