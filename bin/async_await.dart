#!/usr/bin/env dart

// Copyright (c) 2014, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

import 'dart:io';

import 'package:analyzer/src/generated/error.dart';
import 'package:analyzer/src/generated/parser.dart';
import 'package:analyzer/src/generated/scanner.dart';

import 'package:async_await/src/error_collector.dart';
import 'package:async_await/src/pretty_printer.dart';
import 'package:async_await/src/xform.dart';

main(List<String> args) {
  if (args.length != 1) {
    print('Usage: async_await.dart [file]');
    exit(0);
  }

  var errorListener = new ErrorCollector();
  var source = new File(args.first).readAsStringSync();
  var unit = _parse(source, errorListener);
  if (errorListener.errors.isNotEmpty) {
    print("Errors:");
    for (var error in errorListener.errors) {
      print(error);
    }
    exit(1);
  }

  var worklistBuilder = new WorklistBuilder();
  worklistBuilder.visit(unit);
  var transform = new AsyncTransformer();
  var pretty = new PrettyPrinter();
  int position = 0;
  for (var item in worklistBuilder.worklist) {
    pretty.buffer.write(source.substring(position, item.position));
    pretty.visit(transform.visit(item.sourceBody));
    position = item.sourceBody.end;
  }
  pretty.buffer.write(source.substring(position));
  print(pretty.buffer);
}

_parse(String source, AnalysisErrorListener errorListener) {
  var reader = new CharSequenceReader(source);
  var scanner = new Scanner(null, reader, errorListener);
  var token = scanner.tokenize();
  var parser = new Parser(null, errorListener);
  parser.parseAsync = true;
  return parser.parseCompilationUnit(token);
}
