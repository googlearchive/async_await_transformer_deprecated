import 'dart:async';

f(x) async => x;
p(x) async { print(x); }

test0(x) async {
  switch (await f(x)) {
    case 0:
      await p('zero');
      break;
    case 1:
      await p('one');
      break;
    case 2:
      await p('two');
      break;
    default:
      await p('something else');
      break;
  }
  await p('done');
}

test1(x) async {
  switch (await f(x)) {
    L0:
    case 0:
      await p('even');
      break;
    case 1:
      await p('odd');
      break;
    L2:
    case 2:
      await p('even');
      break;
    default:
      await p('something else');
      break;
  }
  await p('done');
}

main() async {
  await test0(0);
  await test0(1);
  await test0(2);
  await test0(3);

  await test1(0);
  await test1(1);
  await test1(2);
  await test1(3);
}
