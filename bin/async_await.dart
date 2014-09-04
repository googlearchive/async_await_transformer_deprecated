#!/usr/bin/env dart

// Copyright (c) 2014, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

import 'dart:io';

import 'package:analyzer/src/generated/error.dart';
import 'package:analyzer/src/generated/parser.dart';
import 'package:analyzer/src/generated/scanner.dart';

import 'package:async_await/src/error_collector.dart';
import 'package:async_await/src/xform.dart';

main(List<String> args) {
  if (args.length != 1) {
    print('Usage: async_await.dart [file]');
    exit(0);
  }

  var errorListener = new ErrorCollector();
  var unit = _parse(new File(args.first), errorListener);
  if (errorListener.errors.isNotEmpty) {
    print("Errors:");
    for (var error in errorListener.errors) {
      print(error);
    }
    exit(1);
  }

  var transform = new AsyncTransformer();
  print(transform.visit(unit));
}

_parse(File file, AnalysisErrorListener errorListener) {
  var src = file.readAsStringSync();
  var reader = new CharSequenceReader(src);
  var scanner = new Scanner(null, reader, errorListener);
  var token = scanner.tokenize();
  var parser = new Parser(null, errorListener);
  parser.parseAsync = true;
  return parser.parseCompilationUnit(token);
}
