// Copyright (c) 2014, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

import 'src/compiler.dart' as compiler;

/// Given [source], a string of Dart code which may contain async/await syntax,
/// compiles it down to raw Dart 1.0 syntax.
String compile(String source) {
  return compiler.compile(source, (errorCollector) {
    throw new FormatException(
        "Compilation error:\n${errorCollector.errors.join("\n")}");
  });
}
