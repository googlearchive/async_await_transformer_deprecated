import 'dart:async';

printAsync(s) async {
   print(s);
}

main() async {
  print("A");
  await printAsync("B");
  print("C");
  await printAsync("D");
  print("E");
}

main() async {
   if(true) {
     await printAsync("A");
   } else {
     print("B");
   }
   print("C");
}

main() async {
   while(true) {
     await printAsync("A");
   }
   print("B");
}

main() async {
   try {
      print("A");
   } catch(e) {
     print("B");
   } finally {
     print("C");
   }
   print("D");
}
