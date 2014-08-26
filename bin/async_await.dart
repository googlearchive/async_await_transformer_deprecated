#!/usr/bin/env dart

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
  var analysis = new AnalysisVisitor();
  analysis.visit(unit);
  var transform = new AsyncTransformer(analysis.awaits);
  print(transform.visit(unit));

  for (var error in errorListener.errors) {
    print(error);
  }
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
