#!/usr/bin/env dart

import 'dart:convert';
import 'dart:io';

import 'package:pretty/pretty.dart' as pretty;

const int LPAREN = 40;
const int RPAREN = 41;

List<String> tokenize(String input) {
  List<String> tokens = <String>[];
  for (String s in input.split(' ')) {
    int i = 0, j = s.length;
    // Add leading parentheses as tokens.
    for (; i < s.length && s.codeUnitAt(i) == LPAREN; ++i) {
      tokens.add('(');
    }

    // Count trailing parentheses.
    for (; j > i && s.codeUnitAt(j - 1) == RPAREN; --j);

    // Add the token itself unless empty.
    if (i < j) {
      tokens.add(s.substring(i, j));
    }

    // Add the trailing parentheses.
    for (; j < s.length; ++j) {
      tokens.add(')');
    }
  }
  return tokens;
}

int position;
List<String> tokens;

buildSexp() {
  String s = tokens[position++];
  return s == '(' ? buildList() : s;
}

List buildList() {
  List result = [];
  while (tokens[position] != ')') {
    result.add(buildSexp());
  }
  ++position;
  return result;
}

pretty.Document sexp2Doc(expression) {
  if (expression is String) return pretty.text(expression);
  if ((expression as List).isEmpty) return pretty.text('()');

  Iterable<pretty.Document> docs = (expression as List).map(sexp2Doc);
  pretty.Document result = pretty.text('(') + docs.first;
  pretty.Document children =
      docs.skip(1).fold(pretty.empty,
          (doc0, doc1) => doc0 + pretty.line + doc1);
  result += (children + pretty.text(')')).nest(2);
  return result.group;
}

void processString(String input) {
  position = 0;
  tokens = tokenize(input);
  print(sexp2Doc(buildSexp()).render(120));
}

main() {
  stdin.transform(ASCII.decoder).listen(processString);
}

