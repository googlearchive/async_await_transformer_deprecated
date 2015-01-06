// Copyright (c) 2014, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

library async_await.src.compiler;

import 'package:analyzer/src/generated/ast.dart';
import 'package:analyzer/src/generated/error.dart';
import 'package:analyzer/src/generated/parser.dart';
import 'package:analyzer/src/generated/scanner.dart';

import 'package:async_await/src/pretty_printer.dart';
import 'package:async_await/src/xform.dart';

CompilationUnit parse(String source, AnalysisErrorListener errorListener) {
  var reader = new CharSequenceReader(source);
  var scanner = new Scanner(null, reader, errorListener);
  var token = scanner.tokenize();
  var parser = new Parser(null, errorListener);
  parser.parseAsync = true;
  parser.parseEnum = true;
  return parser.parseCompilationUnit(token);
}

class ErrorCollector extends AnalysisErrorListener {
  final errors = <AnalysisError>[];
  onError(error) => errors.add(error);
}

String compile(String source, String onError(ErrorCollector errorCollector)) {
  var errorCollector = new ErrorCollector();
  var unit = parse(source, errorCollector);

  if (errorCollector.errors.isNotEmpty) {
    return onError(errorCollector);
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
  return pretty.buffer.toString();
}
