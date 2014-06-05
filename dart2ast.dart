#!/usr/bin/env dart

import 'dart:io';

import 'package:analyzer/analyzer.dart' as ast;

main(List<String> args) {
  if (args.length != 1) {
    print('Usage: dart2ast [Dart file]');
    exit(0);
  }

  String path = args[0];
  ast.CompilationUnit compilationUnit = ast.parseDartFile(path);
  _AstPrintingVisitor visitor = new _AstPrintingVisitor();
  compilationUnit.accept(visitor);
  print(visitor.buffer);
}

class _AstPrintingVisitor extends ast.GeneralizingAstVisitor {
  StringBuffer buffer = new StringBuffer();

  giveup(String why) {
    print('Unsupported syntax: $why.');
    exit(0);
  }

  visitNode(ast.AstNode node) {
    giveup(node.runtimeType.toString());
  }

  visitCompilationUnit(ast.CompilationUnit node) {
    for (ast.AstNode d in node.sortedDirectivesAndDeclarations) {
      d.accept(this);
    }
  }

  visitFunctionDeclaration(ast.FunctionDeclaration node) {
    if (node.name == null) giveup('unnamed function declaration');

    // ==== Tag ====
    String name = node.name.name;
    if (name.contains('_async')) {
      buffer.write('(Async ');
    } else if (name.contains('_syncStar')) {
      buffer.write('(SyncStar ');
    } else {
      buffer.write('(Sync ');
    }

    // ==== Name ====
    buffer.write(name);

    // ==== Parameter list ====
    buffer.write('(');
    ast.FunctionExpression function = node.functionExpression;
    bool first = true;
    for (ast.FormalParameter p in function.parameters.parameters){
      if (!first) buffer.write(' ');
      first = false;
      p.accept(this);
    }
    buffer.write(')');

    // ==== Local variable list ====
    // Here we assume that all variables have been hoisted to the top of the
    // function and declared (but not initialized) in a single statement.
    // We allow there to be no such declaration.
    buffer.write('(');
    if (function.body is! ast.BlockFunctionBody) {
      giveup('not a block function');
    }
    ast.BlockFunctionBody body = function.body;
    ast.Block block = body.block;
    if (!block.statements.isEmpty
        && block.statements[0] is ast.VariableDeclarationStatement) {
      ast.VariableDeclarationStatement decl = block.statements[0];
      bool first = true;
      for (ast.VariableDeclaration d in decl.variables.variables){
        if (!first) buffer.write(' ');
        first = false;
        d.accept(this);
      }
    }
    buffer.write(')');

    first = true;
    for (ast.Statement s in block.statements){
      if (first && s is ast.VariableDeclarationStatement) {
        first = false;
        continue;
      }
      first = false;
      s.accept(this);
    }

    buffer.write(')');
  }

  visitSimpleFormalParameter(ast.SimpleFormalParameter node) {
    buffer.write(node.identifier.name);
  }

  visitVariableDeclaration(ast.VariableDeclaration node) {
    buffer.write(node.name.name);
  }

  // ==== Expressions ====
  visitIntegerLiteral(ast.IntegerLiteral node) {
    buffer.write('(Constant ${node.value})');
  }

  visitSimpleIdentifier(ast.SimpleIdentifier node) {
    buffer.write('(Variable ${node.name})');
  }

  visitAssignmentExpression(ast.AssignmentExpression node) {
    buffer.write('(Assignment');
    node.leftHandSide.accept(this);
    node.rightHandSide.accept(this);
    buffer.write(')');
  }

  visitMethodInvocation(ast.MethodInvocation node) {
    if (node.target != null) giveup("method with a receiver");

    // We don't check that yield and yield* occur as statements.
    String name = node.methodName.name;
    ast.NodeList<ast.Expression> arguments = node.argumentList.arguments;
    String recognizedName;
    if (name == 'await') {
      recognizedName = 'Await';
    } else if (name == 'yield') {
      recognizedName = 'Yield';
    } else if (name == 'yield_star') {
      recognizedName = 'YieldStar';
    }

    if (recognizedName != null) {
      buffer.write('($recognizedName');
      if (arguments.length != 1) giveup('wrong arity for $name');
      arguments[0].accept(this);
    } else {
      buffer.write('(Call ${node.methodName.name}');
      buffer.write('(');
      for (ast.Expression e in node.argumentList.arguments){
        e.accept(this);
      }
      buffer.write(')');
    }
    buffer.write(')');
  }

  visitThrowExpression(ast.ThrowExpression node) {
    buffer.write('(Throw');
    node.expression.accept(this);
    buffer.write(')');
  }

  // ==== Statements ====
  visitBlock(ast.Block node) {
    buffer.write('(Block');
    for (ast.Statement s in node.statements){
      s.accept(this);
    }
    buffer.write(')');
  }

  visitExpressionStatement(ast.ExpressionStatement node) {
    buffer.write('(Expression');
    node.expression.accept(this);
    buffer.write(')');
  }

  visitReturnStatement(ast.ReturnStatement node) {
    // We require a subexpression for return, except for a return from a
    // sync* function, which should have no subexpression.
    if (node.expression == null) {
      buffer.write('(YieldBreak');
    } else {
      buffer.write('(Return');
      node.expression.accept(this);
    }
    buffer.write(')');
  }

  visitIfStatement(ast.IfStatement node) {
    if (node.elseStatement == null) giveup('if without an else');
    buffer.write('(If');
    node.condition.accept(this);
    node.thenStatement.accept(this);
    node.elseStatement.accept(this);
    buffer.write(')');
  }

  visitLabeledStatement(ast.LabeledStatement node) {
    if (node.labels.length != 1) giveup('multiple labels');
    buffer.write('(Label');
    node.statement.accept(this);
    buffer.write(')');
  }

  visitBreakStatement(ast.BreakStatement node) {
    if (node.label == null) giveup('break without a label');
    buffer.write('(Break ${node.label.name})');
  }

  visitWhileStatement(ast.WhileStatement node) {
    buffer.write('(While');
    node.condition.accept(this);
    node.body.accept(this);
    buffer.write(')');
  }

  visitContinueStatement(ast.ContinueStatement node) {
    if (node.label == null) giveup('continue without a label');
    buffer.write('(Continue ${node.label.name})');
  }

  visitTryStatement(ast.TryStatement node) {
    // We support try/catch and try/finally.  Try/catch/finally can be
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
      buffer.write('(TryFinally');
      node.body.accept(this);
      node.finallyBlock.accept(this);
    } else if (node.catchClauses.length == 1) {
      if (node.finallyBlock != null) giveup('try/catch/finally');
      buffer.write('(TryCatch');
      node.body.accept(this);
      ast.CatchClause clause = node.catchClauses[0];
      buffer.write(clause.exceptionParameter.name);
      clause.body.accept(this);
    } else {
      giveup('multiple catch clauses');
    }
    buffer.write(')');
  }

  handleNode(ast.AstNode node) {
    buffer.write('(${node.runtimeType}');
    node.visitChildren(this);
    buffer.write(')');
  }
}
