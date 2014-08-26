library async_await.src.error_collector;

import 'package:analyzer/src/generated/error.dart';

class ErrorCollector extends AnalysisErrorListener {
  final errors = <AnalysisError>[];
  onError(error) => errors.add(error);
}
