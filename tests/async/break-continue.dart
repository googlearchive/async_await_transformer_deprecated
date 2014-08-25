f(n) async => print(n);

test0() async {
  var count = 0;
  while (count < 10) {
    count = count + 1;
    if (count.isOdd) {
      continue;
    }
    await f(count);
  }
}

match(n) async => n == 7;

test1() async {
  var count = 0;
  while (count < 10) {
    if (await match(count)) {
      break;
    }
    count = count + 1;
  }
}


g(n) async { if (n == 7) throw 'seven'; }

test2() async {
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
}

test3() async {
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
}

test4() async {
  var count = 0;
  while (count < 10) {
    try {
      count = count + 1;
      if (await match(n)) {
        continue;
      }
      print(n);
    } finally {
      print('finally');
    }
  }
}

test5() async {
  var count = 0;
  while (count < 10) {
    try {
      count = count + 1;
      if (await match(n)) {
        break;
      }
      print(n);
    } finally {
      print('finally');
    }
  }
}
