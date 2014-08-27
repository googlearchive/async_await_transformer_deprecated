f(x) async => x;

class A { var x; }

main() async {
  final a = new A(), ls = [0], z = 0;
  var x;

  x = await f(0);
  a.x = await f(1);
  (await f(a)).x = 2;
  ls[z] = 3;
  ls[z] = await f(4);
  ls[await f(z)] = 5;
  (await f(ls))[z] = 6;

  x += await f(7);
  a.x += await f(8);
  ls[z] += await f(9);

  x &= await f(10);
  x |= await f(11);
  x ^= await f(12);
  x >>= await f(13);
  x <<= await f(14);
  x -= await f(15);
  x %= await f(16);
  x /= await f(17);
  x *= await f(18);
  x ~/= await f(19);
}
