#!/usr/bin/env dart

import 'dart:io';

import 'package:analyzer/src/generated/error.dart';
import 'package:analyzer/src/generated/parser.dart';
import 'package:analyzer/src/generated/scanner.dart';
import 'xform.dart';

main(List<String> args) {
  if (args.length != 1) {
    print('Usage: async.dart [file]');
    exit(0);
  }

  var errorListener = new _ErrorCollector();
  var unit = _parse(new File(args.first), errorListener);
  var analysis = new AnalysisVisitor();
  analysis.visit(unit);
  var transform = new TransformVisitor(analysis.awaits);
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

class _ErrorCollector extends AnalysisErrorListener {
  List<AnalysisError> errors;
  _ErrorCollector() : errors = new List<AnalysisError>();
  onError(error) => errors.add(error);
}
