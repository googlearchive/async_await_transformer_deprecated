import 'dart:async';

import 'package:barback/barback.dart';

import 'async_await.dart';

/// A [Transformer] that runs the async/await compiler on any .dart files it
/// finds.
class AsyncAwaitTransformer extends Transformer implements LazyTransformer {
  AsyncAwaitTransformer.asPlugin();

  String get allowedExtensions => ".dart";

  void declareOutputs(DeclaringTransform transform) {
    // Just transforms a Dart file in place.
    transform.declareOutput(transform.primaryId);
  }

  Future apply(Transform transform) {
    return transform.primaryInput.readAsString().then((source) {
      source = compile(source);
      transform.addOutput(
          new Asset.fromString(transform.primaryInput.id, source));
    });
  }
}
