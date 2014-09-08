#!/usr/bin/env dart

// Copyright (c) 2014, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

import 'dart:io';

import 'package:async_await/src/compiler.dart';

main(List<String> args) {
  if (args.length != 1) {
    print('Usage: async_await.dart [file]');
    exit(0);
  }
  var source = new File(args.first).readAsStringSync();
  var output = compile(source, (errorCollector) {
    print("Errors:");
    errorCollector.errors.forEach(print);
    exit(1);
  });
  print(output);
}
