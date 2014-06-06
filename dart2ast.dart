#!/usr/bin/env dart

import 'dart:io';

import 'package:analyzer/analyzer.dart' as ast;
import 'package:pretty/pretty.dart' as pretty;

main(List<String> args) {
  if (args.length != 1) {
    print('Usage: dart2ast [Dart file]');
    exit(0);
  }

  String path = args.first;
  ast.CompilationUnit compilationUnit = ast.parseDartFile(path);
  _Ast2SexpVisitor visitor = new _Ast2SexpVisitor();
  print(_sexp2Doc(compilationUnit.accept(visitor)).render(120));
}


_sexp2Doc(expression) {
  if (expression is String) return pretty.text(expression);
  if ((expression as List).isEmpty) return pretty.text('()');

  Iterable<pretty.Document> docs = (expression as List).map(_sexp2Doc);
  pretty.Document result = pretty.text('(') + docs.first;
  pretty.Document children =
      docs.skip(1).fold(pretty.empty,
          (doc0, doc1) => doc0 + pretty.line + doc1);
  result += (children + pretty.text(')')).nest(2);
  return result.group;
}


/// Translate a compilation unit AST to a list of S-expressions.
///
/// An S-expression is an atom (string in this case) or a list of
/// S-expressions.
class _Ast2SexpVisitor extends ast.GeneralizingAstVisitor {
  giveup(String why) {
    print('Unsupported syntax: $why.');
    exit(0);
  }

  visit(ast.AstNode node) => node.accept(this);

  visitNode(ast.AstNode node) {
    giveup(node.runtimeType.toString());
  }

  visitCompilationUnit(ast.CompilationUnit node) {
    return node.sortedDirectivesAndDeclarations.map(visit)
        .toList(growable: false);
  }

  visitFunctionDeclaration(ast.FunctionDeclaration node) {
    if (node.name == null) giveup('unnamed function declaration');

    // ==== Tag and name ====
    String tag;
    String name = node.name.name;
    if (name.contains('_async')) {
      tag = 'Async';
    } else if (name.contains('_syncStar')) {
      tag = 'SyncStar';
    } else {
      tag = 'Sync';
    }

    // ==== Parameter list ====
    ast.FunctionExpression function = node.functionExpression;
    List parameters = function.parameters.parameters.map(visit)
        .toList(growable: false);

    // ==== Local variable list ====
    // Here it is assumed that all variables have been hoisted to the top of
    // the function and declared (but not initialized) in a single statement.
    // There is allowed to be no declaration.
    if (function.body is! ast.BlockFunctionBody) {
      giveup('not a block function');
    }
    ast.Block block = (function.body as ast.BlockFunctionBody).block;
    List locals;
    if (block.statements.isEmpty
        || block.statements.first is! ast.VariableDeclarationStatement) {
      locals = [];
    } else {
      locals = (block.statements.first as ast.VariableDeclarationStatement)
          .variables.variables.map(visit).toList(growable: false);
    }

    bool first = true;
    List body, current;
    for (ast.Statement s in block.statements) {
      if (first && s is ast.VariableDeclarationStatement) {
        // Skip an initial variable declaration statement.
        first = false;
        continue;
      }
      first = false;
      if (current == null) {
        current = body = visit(s);
      } else {
        current = current[current.length - 1] = visit(s);
      }
    }

    return [tag, name, parameters, locals, body];
  }

  visitSimpleFormalParameter(ast.SimpleFormalParameter node) {
    return node.identifier.name;
  }

  visitVariableDeclaration(ast.VariableDeclaration node) {
    return node.name.name;
  }

  // ==== Expressions ====
  visitIntegerLiteral(ast.IntegerLiteral node) {
    return ['Constant', node.value.toString()];
  }

  visitSimpleIdentifier(ast.SimpleIdentifier node) {
    return ['Variable', node.name];
  }

  visitAssignmentExpression(ast.AssignmentExpression node) {
    return ['Assignment', visit(node.leftHandSide),
            visit(node.rightHandSide)];
  }

  visitMethodInvocation(ast.MethodInvocation node) {
    if (node.target != null) giveup("method with a receiver");

    // It is not checked that yield and yield* occur as statements.
    String name = node.methodName.name;
    ast.NodeList<ast.Expression> arguments = node.argumentList.arguments;
    String tag;
    if (name == 'await') {
      tag = 'Await';
    } else if (name == 'yield') {
      tag = 'Yield';
    } else if (name == 'yield_star') {
      tag = 'YieldStar';
    }

    if (tag != null) {
      if (arguments.length != 1) giveup('wrong arity for $name');
      return [tag, visit(arguments.first)];
    } else {
      List arguments = node.argumentList.arguments.map(visit)
          .toList(growable: false);
      return ['Call', name, arguments];
    }
  }

  visitThrowExpression(ast.ThrowExpression node) {
    return ['Throw', visit(node.expression)];
  }

  // ==== Statements ====
  visitBlock(ast.Block node) {
    // Blocks can occur as bodies in if/else, while, try/catch/finally, etc.
    // Since statements in the output are really linked lists of statements
    // terminated by NoStatement, blocks are 'flattened'.
    List result, current;
    for (ast.Statement s in node.statements) {
      if (current == null) {
        current = result = visit(s);
      } else {
        current = current[current.length - 1] = visit(s);
      }
    }
    return result;
  }

  visitExpressionStatement(ast.ExpressionStatement node) {
    return ['Expression', visit(node.expression), 'NoStatement'];
  }

  visitReturnStatement(ast.ReturnStatement node) {
    // A subexpression is required for return, except for a return from a
    // sync* function, which should have no subexpression.
    return node.expression == null
        ? ['YieldBreak', 'NoStatement']
        : ['Return', visit(node.expression), 'NoStatement'];
  }

  visitIfStatement(ast.IfStatement node) {
    if (node.elseStatement == null) giveup('if without an else');
    return ['If', visit(node.condition), visit(node.thenStatement),
            visit(node.elseStatement), 'NoStatement'];
  }

  visitLabeledStatement(ast.LabeledStatement node) {
    // Loops in the output are always labeled.  If the labeled statement is
    // a loop then the label is attached to the loop.
    if (node.labels.length != 1) giveup('multiple labels');
    String label = node.labels.first.label.name;
    List statement = visit(node.statement);
    if (statement.first == 'While') {
      statement[1] = label;
      return statement;
    } else {
      return ['Label', label, statement, 'NoStatement'];
    }
  }

  visitBreakStatement(ast.BreakStatement node) {
    if (node.label == null) giveup('break without a label');
    return ['Break', node.label.name, 'NoStatement'];
  }

  visitWhileStatement(ast.WhileStatement node) {
    // There is a null placeholder for the statement's label.  It is filled
    // in by the caller.
    return ['While', null, visit(node.condition), visit(node.body),
            'NoStatement'];
  }

  visitContinueStatement(ast.ContinueStatement node) {
    if (node.label == null) giveup('continue without a label');
    return ['Continue', node.label.name, 'NoStatement'];
  }

  visitTryStatement(ast.TryStatement node) {
    // Try/catch and try/finally are supported.  Try/catch/finally can be
    // desugared into a language with only try/catch and try/finally:
    //
    // try { S0 } catch (e) { S1 } finally { S2 }
    // ==>
    // try {
    //   try { S0 } catch (e) { S1 }
    // } finally {
    //   S2
    // }
    //
    // It would be relatively simple to do that here, but it's not
    // implemented.
    if (node.catchClauses.isEmpty) {
      // This is probably impossible:
      if (node.finallyBlock == null) giveup('try without catch or finally');
      return ['TryFinally', visit(node.body), visit(node.finallyBlock),
              'NoStatement'];
    } else if (node.catchClauses.length == 1) {
      if (node.finallyBlock != null) giveup('try/catch/finally');
      ast.CatchClause clause = node.catchClauses.first;
      return ['TryCatch', visit(node.body), clause.exceptionParameter.name,
              visit(clause.body), 'NoStatement'];
    } else {
      giveup('multiple catch clauses');
    }
  }
}
