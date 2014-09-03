import 'dart:async';

f(n) async => print(n);

test0() async {
  print('test0');
  var count = 0;
  while (count < 10) {
    count = count + 1;
    if (count.isOdd) {
      continue;
    }
    await f(count);
  }
  print('count = $count');
}

match(n) async => n == 7;

test1() async {
  print('test1');
  var count = 0;
  while (count < 10) {
    if (await match(count)) {
      break;
    }
    count = count + 1;
  }
  print('count = $count');
}


g(n) async { if (n == 7) throw 'seven'; }

test2() async {
  print('test2');
  var count = 0;
  while (count < 10) {
    try {
      await g(count);
      print(count);
      count = count + 1;
    } finally {
      continue;
    }
  }
  print('count = $count');
}

test3() async {
  print('test3');
  var count = 0;
  while (count < 10) {
    try {
      await g(count);
      print(count);
      count = count + 1;
    } finally {
      break;
    }
  }
  print('count = $count');
}

test4() async {
  print('test4');
  var count = 0;
  while (count < 10) {
    try {
      count = count + 1;
      if (await match(count)) {
        continue;
      }
      print(count);
    } finally {
      print('finally');
    }
  }
  print('count = $count');
}

test5() async {
  print('test5');
  var count = 0;
  while (count < 10) {
    try {
      count = count + 1;
      if (await match(count)) {
        break;
      }
      print(count);
    } finally {
      print('finally');
    }
  }
  print('count = $count');
}

main() async {
  await test0();
  await test1();
  // await test2();
  await test3();
  await test4();
  await test5();
}
