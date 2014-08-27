import 'package:analyzer/src/generated/ast.dart';
import 'package:analyzer/src/generated/error.dart';
import 'package:analyzer/src/generated/parser.dart';
import 'package:analyzer/src/generated/scanner.dart';

import 'src/error_collector.dart';
import 'src/xform.dart';

/// Given [source], a string of Dart code which may contain async/await syntax,
/// compiles it down to raw Dart 1.0 syntax.
String compile(String source) {
  var errorListener = new ErrorCollector();
  var unit = _parse(source, errorListener);

  if (errorListener.errors.isNotEmpty) {
    throw new FormatException(
      "Compilation error:\n${errorListener.errors.join("\n")}");
  }

  var transform = new AsyncTransformer();
  return transform.visit(unit).toString();
}

CompilationUnit _parse(String source, AnalysisErrorListener errorListener) {
  var reader = new CharSequenceReader(source);
  var scanner = new Scanner(null, reader, errorListener);
  var token = scanner.tokenize();
  var parser = new Parser(null, errorListener);
  parser.parseAsync = true;
  return parser.parseCompilationUnit(token);
}
