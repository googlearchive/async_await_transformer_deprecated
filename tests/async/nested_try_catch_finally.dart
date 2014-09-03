import 'dart:async';

main() async {
  await f();
}

S(n, { throws: false }) async {
  print(n);
  if (throws) throw n;
}

f() async {
  try {
    try {  
      await S(1, throws: true);
    } catch(e){  
      await S(2, throws: true);
    } finally { 
      await S(3, throws: true);
    }
  } catch(e) {
    try {
      await S(4, throws: true);
    } catch(e) {
      await S(5, throws: true);
    } finally {
      await S(6, throws: true);
    }
  } finally {
    try {
      await S(7, throws: true);
    } catch(e) { 
      await S(8, throws: true);
    } finally {
      await S(9);
    }
  }
}
