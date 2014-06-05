import 'dart:collection';

void main() {
   print(map([0,1,2,3,4,5], (x)=>x%2==0);
}

map(ss, f) sync* {
  for(var s in ss) yield f(s);
}