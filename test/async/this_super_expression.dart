import 'dart:async';

class A {
  f() async {
     return await this.g();
  }

  g() async {
    return this;
  }
}

class B extends A {
  h() async {
     return await super.f();
  }
}

main() async {
   await new B().h();
}
