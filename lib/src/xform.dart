// Copyright (c) 2014, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

library async_await.src.xform;

import 'package:analyzer/src/generated/ast.dart' as ast;
import 'package:analyzer/src/generated/scanner.dart' as scanner;
import 'package:analyzer/src/generated/testing/ast_factory.dart';
import 'package:analyzer/src/generated/testing/token_factory.dart';

class WorklistItem {
  final int position;
  final ast.FunctionBody sourceBody;

  WorklistItem(this.position, this.sourceBody);
}

class WorklistBuilder extends ast.RecursiveAstVisitor {
  final List<WorklistItem> worklist = <WorklistItem>[];
  visit(ast.AstNode node) => node.accept(this);

  visitExpressionFunctionBody(ast.ExpressionFunctionBody node) {
    if (node.isAsynchronous || node.isGenerator) {
      worklist.add(new WorklistItem(node.keyword.offset, node));
    } else {
      super.visitExpressionFunctionBody(node);
    }
  }

  visitBlockFunctionBody(ast.BlockFunctionBody node) {
    if (node.isAsynchronous || node.isGenerator) {
      worklist.add(new WorklistItem(node.keyword.offset, node));
    } else {
      super.visitBlockFunctionBody(node);
    }
  }
}

class Analysis extends ast.GeneralizingAstVisitor<bool> {
  Set<ast.AstNode> awaits = new Set<ast.AstNode>();
  Map<ast.Statement, List<ast.Label>> labels =
      <ast.Statement, List<ast.Label>>{};
  Set<String> names = new Set<String>();

  bool maybeAdd(ast.AstNode node, bool shouldAdd) {
    if (shouldAdd) awaits.add(node);
    return shouldAdd;
  }

  bool visit(ast.AstNode node) => node.accept(this);

  bool visitNode(ast.AstNode node) {
    throw 'Analysis: unreachable(${node.runtimeType})';
  }

  bool visitArgumentList(ast.ArgumentList node) {
    var result = false;
    for (var e in node.arguments) {
      if (visit(e)) result = true;
    }
    return maybeAdd(node, result);
  }

  bool visitCatchClause(ast.CatchClause node) {
    return maybeAdd(node, visit(node.body));
  }

  bool visitVariableDeclarationList(ast.VariableDeclarationList node) {
    var result = false;
    for (var d in node.variables) {
      if (visit(d)) result = true;
    }
    return maybeAdd(node, result);
  }

  bool visitVariableDeclaration(ast.VariableDeclaration node) {
    names.add(node.name.name);
    if (node.initializer == null) return false;
    return maybeAdd(node, visit(node.initializer));
  }

  // ---- Statements ----
  bool visitAssertStatement(ast.AssertStatement node) {
    return maybeAdd(node, visit(node.condition));
  }

  bool visitBlock(ast.Block node) {
    var result = false;
    for (var s in node.statements) {
      if (visit(s)) result = true;
    }
    return maybeAdd(node, result);
  }

  bool visitBreakStatement(ast.BreakStatement node) {
    return false;
  }

  bool visitContinueStatement(ast.ContinueStatement node) {
    return false;
  }

  bool visitDoStatement(ast.DoStatement node) {
    var result = visit(node.body);
    return maybeAdd(node, visit(node.condition) || result);
  }

  bool visitEmptyStatement(ast.EmptyStatement node) {
    return false;
  }

  bool visitExpressionStatement(ast.ExpressionStatement node) {
    return maybeAdd(node, visit(node.expression));
  }

  bool visitForEachStatement(ast.ForEachStatement node) {
    if (node.identifier != null) {
      names.add(node.identifier.name);
    } else {
      assert(node.loopVariable != null);
      names.add(node.loopVariable.identifier.name);
    }
    var result = visit(node.iterator);
    return maybeAdd(node, visit(node.body) || result);
  }

  bool visitForStatement(ast.ForStatement node) {
    var result = false;
    if (node.variables != null) {
      result = visit(node.variables);
    } else if (node.initialization != null) {
      result = visit(node.initialization);
    }
    if (node.condition != null) {
      if (visit(node.condition)) result = true;
    }
    if (visit(node.body)) result = true;
    if (node.updaters != null) {
      for (var e in node.updaters) {
        if (visit(e)) result = true;
      }
    }
    return maybeAdd(node, result);
  }

  bool visitFunctionDeclarationStatement(
      ast.FunctionDeclarationStatement node) {
    return false;
  }

  bool visitIfStatement(ast.IfStatement node) {
    var result = visit(node.condition);
    if (visit(node.thenStatement)) result = true;
    if (node.elseStatement != null) {
      if (visit(node.elseStatement)) result = true;
    }
    return maybeAdd(node, result);
  }

  bool visitLabeledStatement(ast.LabeledStatement node) {
    labels[node.statement] = node.labels;
    return maybeAdd(node, visit(node.statement));
  }

  bool visitRethrowExpression(ast.RethrowExpression node) {
    return false;
  }

  bool visitReturnStatement(ast.ReturnStatement node) {
    if (node.expression == null) return false;
    return maybeAdd(node, visit(node.expression));
  }

  bool visitSwitchCase(ast.SwitchCase node) {
    var result = visit(node.expression);
    for (var s in node.statements) {
      if (visit(s)) result = true;
    }
    return maybeAdd(node, result);
  }

  bool visitSwitchDefault(ast.SwitchDefault node) {
    var result = false;
    for (var s in node.statements) {
      if (visit(s)) result = true;
    }
    return maybeAdd(node, result);
  }

  bool visitSwitchStatement(ast.SwitchStatement node) {
    var result = visit(node.expression);
    for (var m in node.members) {
      if (visit(m)) result = true;
    }
    return maybeAdd(node, result);
  }

  bool visitTryStatement(ast.TryStatement node) {
    var result = visit(node.body);
    for (var clause in node.catchClauses) {
      if (visit(clause)) result = true;
    }
    if (node.finallyBlock != null) {
      if (visit(node.finallyBlock)) result = true;
    }
    return maybeAdd(node, result);
  }

  bool visitVariableDeclarationStatement(
        ast.VariableDeclarationStatement node) {
    return maybeAdd(node, visit(node.variables));
  }

  bool visitWhileStatement(ast.WhileStatement node) {
    var result = visit(node.condition);
    return maybeAdd(node, visit(node.body) || result);
  }

  bool visitYieldStatement(ast.YieldStatement node) {
    return maybeAdd(node, visit(node.expression));
  }

  // ---- Expressions ----
  bool visitExpression(ast.Expression node) {
    throw 'Analysis: unimplemented(${node.runtimeType})';
  }

  bool visitAdjacentStrings(ast.AdjacentStrings node) {
    var result = false;
    for (var s in node.strings) {
      if (visit(s)) result = true;
    }
    return maybeAdd(node, result);
  }

  bool visitAsExpression(ast.AsExpression node) {
    return maybeAdd(node, visit(node.expression));
  }

  bool visitAssignmentExpression(ast.AssignmentExpression node) {
    var result = visit(node.leftHandSide);
    return maybeAdd(node, visit(node.rightHandSide) || result);
  }

  bool visitAwaitExpression(ast.AwaitExpression node) {
    visit(node.expression);
    awaits.add(node);
    return true;
  }

  bool visitBinaryExpression(ast.BinaryExpression node) {
    var result = visit(node.leftOperand);
    return maybeAdd(node, visit(node.rightOperand) || result);
  }

  bool visitBooleanLiteral(ast.BooleanLiteral node) {
    return false;
  }

  bool visitCascadeExpression(ast.CascadeExpression node) {
    var result = visit(node.target);
    for (var s in node.cascadeSections) {
      if (visit(s)) result = true;
    }
    return maybeAdd(node, result);
  }

  bool visitConditionalExpression(ast.ConditionalExpression node) {
    var result = visit(node.condition);
    if (visit(node.thenExpression)) result = true;
    return maybeAdd(node, visit(node.elseExpression) || result);
  }

  bool visitDoubleLiteral(ast.DoubleLiteral node) {
    return false;
  }

  bool visitFunctionExpression(ast.FunctionExpression node) {
    return false;
  }

  bool visitIndexExpression(ast.IndexExpression node) {
    var result = (node.target != null) && visit(node.target);
    return maybeAdd(node, visit(node.index) || result);
  }

  bool visitInstanceCreationExpression(ast.InstanceCreationExpression node) {
    return maybeAdd(node, visit(node.argumentList));
  }

  bool visitIntegerLiteral(ast.IntegerLiteral node) {
    return false;
  }

  bool visitIsExpression(ast.IsExpression node) {
    return maybeAdd(node, visit(node.expression));
  }

  bool visitFunctionExpressionInvocation(
      ast.FunctionExpressionInvocation node) {
    var result = visit(node.function);
    return maybeAdd(node, visit(node.argumentList) || result);
  }

  bool visitListLiteral(ast.ListLiteral node) {
    var result = false;
    for (var e in node.elements) {
      if (visit(e)) result = true;
    }
    return maybeAdd(node, result);
  }

  bool visitMapLiteral(ast.MapLiteral node) {
    var result = false;
    for (var entry in node.entries) {
      if (visit(entry)) result = true;
    }
    return maybeAdd(node, result);
  }

  bool visitMapLiteralEntry(ast.MapLiteralEntry node) {
    var result = visit(node.key);
    return maybeAdd(node, visit(node.value) || result);
  }

  bool visitMethodInvocation(ast.MethodInvocation node) {
    var result = node.target != null && visit(node.target);
    return maybeAdd(node, visit(node.argumentList) || result);
  }

  bool visitNamedExpression(ast.NamedExpression node) {
    return maybeAdd(node, visit(node.expression));
  }

  bool visitNullLiteral(ast.NullLiteral node) {
    return false;
  }

  bool visitParenthesizedExpression(ast.ParenthesizedExpression node) {
    return maybeAdd(node, visit(node.expression));
  }

  bool visitPostfixExpression(ast.PostfixExpression node) {
    return maybeAdd(node, visit(node.operand));
  }

  bool visitPrefixedIdentifier(ast.PrefixedIdentifier node) {
    names.add(node.prefix.name);
    names.add(node.identifier.name);
    return false;
  }

  bool visitPrefixExpression(ast.PrefixExpression node) {
    return maybeAdd(node, visit(node.operand));
  }

  bool visitPropertyAccess(ast.PropertyAccess node) {
    return maybeAdd(node, node.target != null && visit(node.target));
  }

  bool visitSimpleStringLiteral(ast.SimpleStringLiteral node) {
    return false;
  }

  bool visitSimpleIdentifier(ast.SimpleIdentifier node) {
    names.add(node.name);
    return false;
  }

  bool visitSuperExpression(ast.SuperExpression node) {
    return false;
  }

  bool visitThisExpression(ast.ThisExpression node) {
    return false;
  }

  bool visitInterpolationExpression(ast.InterpolationExpression node) {
    return maybeAdd(node, visit(node.expression));
  }

  bool visitInterpolationString(ast.InterpolationString node) {
    return false;
  }

  bool visitStringInterpolation(ast.StringInterpolation node) {
    var result = false;
    for (var elt in node.elements) {
      if (visit(elt)) result = true;
    }
    return maybeAdd(node, result);
  }

  bool visitSymbolLiteral(ast.SymbolLiteral node) {
    return false;
  }

  bool visitThrowExpression(ast.ThrowExpression node) {
    return maybeAdd(node, visit(node.expression));
  }
}

// AST construction functions.
ast.SimpleIdentifier identifier(String name) {
  return AstFactory.identifier3(name);
}

ast.FunctionExpression functionExpression(List<String> parameters,
    ast.AstNode body) {
  var formalParameters =
      AstFactory.formalParameterList(
          parameters.map(AstFactory.simpleFormalParameter3).toList());
  var functionBody;
  if (body is ast.Block) {
    functionBody = AstFactory.blockFunctionBody(body);
  } else if (body is ast.Statement) {
    functionBody = AstFactory.blockFunctionBody(AstFactory.block([body]));
  } else {
    assert(body is ast.Expression);
    functionBody = AstFactory.expressionFunctionBody(body);
    functionBody.semicolon = null;
  }
  return AstFactory.functionExpression2(formalParameters, functionBody);
}

ast.VariableDeclarationStatement variableDeclaration(
    String name, ast.Expression initializer) {
  return AstFactory.variableDeclarationStatement2(
          scanner.Keyword.FINAL,
          [AstFactory.variableDeclaration2(name, initializer)]);
}

ast.Block emptyBlock() {
  return AstFactory.block([]);
}

ast.Block block(List<ast.AstNode> body) {
  var stmts = body.map((node) {
    if (node is ast.Expression) {
      return AstFactory.expressionStatement(node);
    } else {
      assert(node is ast.Statement);
      return node;
    }
  }).toList();
  return AstFactory.block(stmts);
}

ast.NullLiteral nullLiteral() {
  return AstFactory.nullLiteral();
}

ast.MethodInvocation methodInvocation(receiver,
                                      String name,
                                      List<ast.AstNode> args) {
  if (receiver == null) {
    return AstFactory.methodInvocation2(name, args);
  } else if (receiver is String) {
    return AstFactory.methodInvocation(identifier(receiver), name, args);
  } else {
    assert(receiver is ast.Expression);
    return AstFactory.methodInvocation(receiver, name, args);
  }
}

class AsyncTransformer extends ast.AstVisitor {
  Set<ast.AstNode> awaits;
  Map<ast.Statement, List<ast.Label>> labels;
  Set<String> names;

  ast.Block currentBlock;
  List<ast.Expression> breakTargets;
  List<String> breakLabels;

  List<ast.Expression> continueTargets;
  List<String> continueLabels;

  visit(ast.AstNode node) => node.accept(this);

  Map<String, int> nameCounters;

  String newName(String base) {
    if (!nameCounters.containsKey(base)) nameCounters[base] = 0;
    var name;
    do {
      name = '$base${nameCounters[base]++}';
    } while (names.contains(name));
    return name;
  }

  void reset(Analysis analysis) {
    nameCounters = <String, int>{};
    currentBlock = emptyBlock();
    breakTargets = <ast.Expression>[];
    breakLabels = <String>[];
    continueTargets = <ast.Expression>[];
    continueLabels = <String>[];
    awaits = analysis.awaits;
    labels = analysis.labels;
    names = analysis.names;
  }

  /// Insert a declaration with initial value [expr] in [currentBlock] and
  /// return the fresh name.
  ///
  /// No declaration is added if the expression is already a value and [force]
  /// is false.
  ast.Expression addTempDeclaration(ast.Expression expr) {
    if (expr is ast.Literal && expr is! ast.TypedLiteral) {
      return expr;
    }
    var name = newName('v');
    addStatement(variableDeclaration(name, expr));
    return identifier(name);
  }

  void addStatement(ast.AstNode node) {
    if (node is ast.Statement) {
      currentBlock.statements.add(node);
    } else {
      assert(node is ast.Expression);
      currentBlock.statements.add(AstFactory.expressionStatement(node));
    }
  }

  ast.FunctionExpression reifyExpressionCont(f, baseName) {
    var savedBlock = currentBlock;
    var bodyBlock = currentBlock = emptyBlock();
    String name = newName(baseName);
    f(identifier(name));
    currentBlock = savedBlock;
    return functionExpression([name], bodyBlock);
  }

  ast.AstNode applyExpressionCont(f, v) {
    v = addTempDeclaration(v);
    var savedBlock = currentBlock;
    var bodyBlock = currentBlock = emptyBlock();
    f(v);
    currentBlock = savedBlock;

    var body = bodyBlock;
    if (body.statements.length == 1 &&
        body.statements.first is ast.ReturnStatement) {
      body = (body.statements.first as ast.ReturnStatement).expression;
      if (body == null) body = nullLiteral();
    }
    return body;
  }

  ast.FunctionExpression reifyStatementCont(s) {
    var savedBlock = currentBlock;
    currentBlock = emptyBlock();
    s();
    var fun = functionExpression([], currentBlock);
    currentBlock = savedBlock;
    return fun;
  }

  // ---- CompilationUnit ----
  visitCompilationUnit(ast.CompilationUnit node) {
    // TODO(kmillikin): import 'dart:async' if necessary.
    return new ast.CompilationUnit(
        node.beginToken,
        node.scriptTag,
        node.directives,
        node.declarations.map(visit).toList(),
        node.endToken);
  }

  // ---- CompilationUnitMembers ----
  visitClassDeclaration(ast.ClassDeclaration node) {
    return new ast.ClassDeclaration(
        node.documentationComment,
        node.metadata,
        node.abstractKeyword,
        node.classKeyword,
        node.name,
        node.typeParameters,
        node.extendsClause,
        node.withClause,
        node.implementsClause,
        node.leftBracket,
        node.members.map(visit).toList(),
        node.rightBracket);
  }

  visitClassTypeAlias(ast.ClassTypeAlias node) {
    return node;
  }

  visitEnumDeclaration(ast.EnumDeclaration node) {
    return node;
  }

  visitFunctionDeclaration(ast.FunctionDeclaration node) {
    node.functionExpression.body = visit(node.functionExpression.body);
    return node;
  }

  visitFunctionTypeAlias(ast.FunctionTypeAlias node) {
    return node;
  }

  visitTopLevelVariableDeclaration(ast.TopLevelVariableDeclaration node) {
    return node;
  }

  // ---- ClassMembers ----
  visitConstructorDeclaration(ast.ConstructorDeclaration node) {
    return node;
  }

  visitFieldDeclaration(ast.FieldDeclaration node) {
    return node;
  }

  visitMethodDeclaration(ast.MethodDeclaration node) {
    node.body = visit(node.body);
    return node;
  }

  // ---- FunctionBodies ----
  visitBlockFunctionBody(ast.BlockFunctionBody node) {
    if (node.isGenerator) {
      throw 'Transfomer: unsupported generator function.';
    }
    if (node.isSynchronous) {
      new SyncTransformer().visit(node);
      return node;
    }

    Analysis analysis = new Analysis();
    analysis.visit(node.block);
    reset(analysis);

    visit(node.block)((v) {
      addStatement(AstFactory.returnStatement2(v));
    }, () {
      addStatement(AstFactory.returnStatement2(nullLiteral()));
    });

    return AstFactory.blockFunctionBody2(
        [AstFactory.returnStatement2(
             AstFactory.instanceCreationExpression2(
                 scanner.Keyword.NEW,
                 AstFactory.typeName3(
                     AstFactory.identifier5('Future', 'microtask'), []),
                 [functionExpression([], currentBlock)]))]);
  }

  visitEmptyFunctionBody(ast.EmptyFunctionBody node) {
    return node;
  }

  visitExpressionFunctionBody(ast.ExpressionFunctionBody node) {
    if (node.isSynchronous || node.isGenerator) return node;
    return visit(AstFactory.blockFunctionBody2(
        [AstFactory.returnStatement2(node.expression)])
        ..keyword = node.keyword);
  }

  visitNativeFunctionBody(ast.NativeFunctionBody node) {
    return node;
  }

  // ---- Statements ----
  unimplemented(ast.AstNode node) => throw 'Unimplemented(${node.runtimeType})';

  visitAssertStatement(ast.AssertStatement node) => (r, s) {
    return visit(node.condition)((cond) {
      addStatement(AstFactory.assertStatement(cond));
      return s();
    });
  };

  _translateStatementList(ast.NodeList<ast.Statement> list, r, s) {
    for (var stmt in list.reversed) {
      var nextCont = s;
      s = () => visit(stmt)(r, nextCont);
    }
    return s();
  }

  visitBlock(ast.Block node) => (r, s) {
    return _translateStatementList(node.statements, r, s);
  };

  ast.Expression _findJumpTarget(node, List<ast.Expression> targets,
                                 List<String> names) {
    if (node.label == null) {
      return targets.last;
    }
    for (var i = targets.length - 1; i >= 0; --i) {
      if (names[i] == node.label.name) return targets[i];
    }
    throw 'Illegal ${node.runtimeType}: $node';
  }

  visitBreakStatement(ast.BreakStatement node) => (r, s) {
    var target = _findJumpTarget(node, breakTargets, breakLabels);
    addStatement(AstFactory.returnStatement2(target));
  };

  visitContinueStatement(ast.ContinueStatement node) => (r, s) {
    var target = _findJumpTarget(node, continueTargets, continueLabels);
    addStatement(AstFactory.returnStatement2(target));
  };

  _addJumpTargets(ast.AstNode node, String name, List<ast.Expression> targets,
                  List<String> names) {
    var target =
        AstFactory.functionExpressionInvocation(identifier(name), []);
    if (labels[node] == null) {
      targets.add(target);
      names.add(null);
    } else {
      for (var label in labels[node]) {
        targets.add(target);
        names.add(label.label.name);
      }
    }
  }

  _removeJumpTargets(ast.AstNode node, List<ast.Expression> targets,
                     List<String> names) {
    if (labels[node] == null) {
      targets.removeLast();
      names.removeLast();
    } else {
      var delta = labels[node].length;
      targets.length -= delta;
      names.length -= delta;
    }
  }

  visitDoStatement(ast.DoStatement node) => (r, s) {
    var breakName = newName('break');
    var continueName = newName('continue');
    var loopName = newName('loop');

    var savedBlock = currentBlock;
    var breakBlock = currentBlock = emptyBlock();
    s();

    var continueBlock = currentBlock = emptyBlock();
    visit(node.condition)((expr) {
      addStatement(AstFactory.ifStatement2(
          expr,
          block([AstFactory.returnStatement2(AstFactory.methodInvocation(
              AstFactory.functionExpressionInvocation(
                  AstFactory.identifier(identifier('Future'),
                                        identifier('wait')),
                  [AstFactory.listLiteral([])]),
              'then',
              [functionExpression([newName('x')],
                   AstFactory.functionExpressionInvocation(
                       identifier(loopName), []))]))]),
          block([AstFactory.returnStatement2(
              AstFactory.functionExpressionInvocation(
                  identifier(breakName), []))])));
    });

    var loopBlock = currentBlock = emptyBlock();
    addStatement(
        AstFactory.functionDeclarationStatement(null, null, continueName,
            functionExpression([], continueBlock)));
    _addJumpTargets(node, breakName, breakTargets, breakLabels);
    _addJumpTargets(node, continueName, continueTargets, continueLabels);
    visit(node.body)(r, () {
      addStatement(AstFactory.returnStatement2(
          AstFactory.functionExpressionInvocation(
              identifier(continueName), [])));
    });
    _removeJumpTargets(node, breakTargets, breakLabels);
    _removeJumpTargets(node, continueTargets, continueLabels);

    currentBlock = savedBlock;
    addStatement(
        AstFactory.functionDeclarationStatement(null, null, breakName,
            functionExpression([], breakBlock)));
    addStatement(
        AstFactory.functionDeclarationStatement(null, null, loopName,
            functionExpression([], loopBlock)));
    addStatement(AstFactory.returnStatement2(
        AstFactory.functionExpressionInvocation(
            identifier(loopName), [])));
  };

  visitEmptyStatement(ast.EmptyStatement node) => (r, s) {
    return s();
  };

  visitExpressionStatement(ast.ExpressionStatement node) => (r, s) {
    return visit(node.expression)((expr) {
      addStatement(expr);
      return s();
    });
  };

  visitForEachStatement(ast.ForEachStatement node) {
    var stmt = emptyBlock();
    var it = newName('it');
    stmt.statements.add(
        AstFactory.variableDeclarationStatement2(
            scanner.Keyword.VAR,
            [AstFactory.variableDeclaration2(it,
                AstFactory.propertyAccess2(node.iterator, 'iterator'))]));
    var body;
    if (node.identifier != null) {
      body = block(
          [AstFactory.assignmentExpression(
               node.identifier,
               scanner.TokenType.EQ,
               AstFactory.propertyAccess2(identifier(it), 'current')),
           node.body]);
    } else {
      assert(node.loopVariable != null);
      body = block(
          [AstFactory.variableDeclarationStatement2(
              scanner.Keyword.keywords[node.loopVariable.keyword.lexeme],
              [AstFactory.variableDeclaration2(
                   node.loopVariable.identifier.name,
                   AstFactory.propertyAccess2(identifier(it), 'current'))]),
           node.body]);
    }
    stmt.statements.add(AstFactory.whileStatement(
        AstFactory.methodInvocation(identifier(it), 'moveNext', []),
        body));
    return visitBlock(stmt);
  }

  _translateForUpdaters(List<ast.Expression> exprs, s) {
    var cont = s;
    for (var expr in exprs.reversed) {
      var nextCont = cont;
      cont = () {
        visit(expr)((expr) {
          addStatement(expr);
          return nextCont();
        });
      };
     }
     return cont();
   }

  _translateForDeclarations(List<ast.VariableDeclaration> decls, s) {
    var exprs = [];
    var seenAwait = false;
    var cont = (e) {
      exprs.add(e);
      return s(exprs);
    };
    for (var i = decls.length - 1; i >= 1; --i) {
      // Build the continuation for the i-1 initializer expression.
      var nextExpr = decls[i].initializer;
      if (nextExpr != null) {
        seenAwait = seenAwait || awaits.contains(nextExpr);
      }
      var nextCont = cont;
      var copiedSeenAwait = seenAwait;
      cont = (e) {
        if (copiedSeenAwait) e = addTempDeclaration(e);
        exprs.add(e);
        return (nextExpr == null)
            ? nextCont(nullLiteral())
            : visit(nextExpr)(nextCont);
      };
    }
    var expr = decls.first.initializer;
    return (expr == null)
        ? cont(nullLiteral())
        :  visit(expr)(cont);
  }

 visitForStatement(ast.ForStatement node) => (r, s) {
    var breakName = newName('break');
    var continueName = newName('continue');
    var loopName = newName('loop');

    var savedBlock = currentBlock;
    var breakBlock = currentBlock = emptyBlock();
    s();

    var parameters;
    if (node.variables != null) {
      parameters = node.variables.variables.map((d) => d.name).toList();
    } else {
      parameters = <ast.SimpleIdentifier>[];
    }

    var continueBlock = currentBlock = emptyBlock();
    trampoline() {
      addStatement(AstFactory.returnStatement2(
          AstFactory.methodInvocation(
              AstFactory.functionExpressionInvocation(
                  AstFactory.identifier(identifier('Future'),
                                        identifier('wait')),
                  [AstFactory.listLiteral([])]),
              'then',
              [functionExpression ([newName('x')],
                  AstFactory.functionExpressionInvocation(
                      identifier(loopName),
                      parameters))])));
    }
    if (node.updaters != null) {
      _translateForUpdaters(node.updaters, trampoline);
    } else {
      trampoline();
    }

    var bodyBlock = currentBlock = emptyBlock();
    _addJumpTargets(node, breakName, breakTargets, breakLabels);
    _addJumpTargets(node, continueName, continueTargets, continueLabels);
    visit(node.body)(r, () {
      addStatement(AstFactory.returnStatement2(
          AstFactory.functionExpressionInvocation(
              identifier(continueName), [])));
    });
    _removeJumpTargets(node, breakTargets, breakLabels);
    _removeJumpTargets(node, continueTargets, continueLabels);

    var loopBlock = currentBlock = emptyBlock();
    addStatement(
        AstFactory.functionDeclarationStatement(null, null, continueName,
            functionExpression([], continueBlock)));
    if (node.condition != null) {
      visit(node.condition)((expr) {
        addStatement(AstFactory.ifStatement2(
          expr,
          bodyBlock,
          block([AstFactory.returnStatement2(
              AstFactory.functionExpressionInvocation(
                  identifier(breakName), []))])));
      });
    } else {
      addStatement(bodyBlock);
    }

    currentBlock = savedBlock;
    addStatement(
        AstFactory.functionDeclarationStatement(null, null, breakName,
            functionExpression([], breakBlock)));
    addStatement(
        AstFactory.functionDeclarationStatement(null, null, loopName,
            functionExpression(parameters.map((e) => e.name).toList(),
                loopBlock)));
    if (node.variables != null) {
      assert(node.variables.variables.isNotEmpty);
      return _translateForDeclarations(node.variables.variables, (args) {
        assert(args.length == parameters.length);
        addStatement(AstFactory.returnStatement2(
            AstFactory.functionExpressionInvocation(
                identifier(loopName), args)));
      });
    } else if (node.initialization != null) {
      assert(parameters.isEmpty);
      return visit(node.initialization)((expr) {
        addStatement(expr);
        addStatement(AstFactory.returnStatement2(
            AstFactory.functionExpressionInvocation(
                identifier(loopName), [])));
      });
    } else {
      assert(parameters.isEmpty);
      return addStatement(AstFactory.returnStatement2(
          AstFactory.functionExpressionInvocation(
              identifier(loopName), [])));
    }
  };

  visitFunctionDeclarationStatement(
      ast.FunctionDeclarationStatement node) => (r, s) {
    var decl = new AsyncTransformer().visit(node.functionDeclaration);
    addStatement(new ast.FunctionDeclarationStatement(decl));
    return s();
  };

  visitIfStatement(ast.IfStatement node) => (r, s) {
    var hasElse = node.elseStatement != null;
    return visit(node.condition)((expr) {
      var joinName = newName('join');
      var joinFun = reifyStatementCont(s);
      addStatement(
          AstFactory.functionDeclarationStatement(
              null, null, joinName, joinFun));
      s = () {
        addStatement(AstFactory.returnStatement2(
            AstFactory.methodInvocation2(joinName, [])));
      };
      var savedBlock = currentBlock;
      var thenBlock = currentBlock = emptyBlock();
      visit(node.thenStatement)(r, s);
      var elseBlock = currentBlock = emptyBlock();
      if (hasElse) {
        visit(node.elseStatement)(r, s);
      } else {
        s();
      }
      currentBlock = savedBlock;
      addStatement(AstFactory.ifStatement2(expr, thenBlock, elseBlock));
    });
  };

  bool _isLoop(ast.AstNode node) {
    return node is ast.DoStatement ||
        node is ast.ForEachStatement ||
        node is ast.ForStatement ||
        node is ast.WhileStatement;
  }

  visitLabeledStatement(ast.LabeledStatement node) {
    var stmt = node.statement;
    if (_isLoop(stmt) || stmt is ast.SwitchStatement) {
      return visit(stmt);
    }
    return (r, s) {
      var breakName = newName('break');
      var savedBlock = currentBlock;
      var breakBlock = currentBlock = emptyBlock();
      s();
      currentBlock = savedBlock;

      addStatement(
          AstFactory.functionDeclarationStatement(
            null,
            null,
            breakName,
            functionExpression([newName('x')], breakBlock)));

      _addJumpTargets(stmt, breakName, breakTargets, breakLabels);
      visit(stmt)(r, () {
        addStatement(AstFactory.returnStatement2(
          AstFactory.functionExpressionInvocation(
              identifier(breakName), [nullLiteral()])));
      });
      _removeJumpTargets(stmt, breakTargets, breakLabels);
    };
  }

  visitRethrowExpression(ast.RethrowExpression node) => unimplemented(node);

  visitReturnStatement(ast.ReturnStatement node) => (r, s) {
    return (node.expression == null)
        ? r(AstFactory.nullLiteral())
        : visit(node.expression)((v) => r(v));
  };

  visitSwitchStatement(ast.SwitchStatement node) => (r, s) {
    return visit(node.expression)((expr) {
      var breakName = newName('break');
      ast.FunctionExpression breakFun = reifyStatementCont(s);
      addStatement(
          AstFactory.functionDeclarationStatement(
              null, null, breakName, breakFun));

      // Generate a name per labeled case.
      var continueNames = [];
      var savedLength = continueTargets.length;
      for (var member in node.members) {
        var labels = member.labels;
        if (labels.isNotEmpty) {
          var continueName = newName('continue');
          continueNames.add(continueName);
          continueTargets.addAll(labels.map((_) {
            return AstFactory.functionExpressionInvocation(
                identifier(continueName), []);
          }));
          continueLabels.addAll(labels.map((lbl) => lbl.label.name));
        }
      }

      // Translate the cases with bindings for the break and possible
      // continues.
      _addJumpTargets(node, breakName, breakTargets, breakLabels);
      if (continueNames.isNotEmpty) {
        // Add declarations for mutable continue functions.
        addStatement(
            AstFactory.variableDeclarationStatement2(scanner.Keyword.VAR,
                continueNames.map(AstFactory.variableDeclaration).toList()));

        // Translate the labeled cases as recursive functions.
        var index = 0;
        for (var member in node.members) {
          if (member.labels.isEmpty) continue;
          var savedBlock = currentBlock;
          var caseBlock = currentBlock = emptyBlock();
          _translateStatementList(member.statements, r, () {
            addStatement(AstFactory.returnStatement2(
                AstFactory.functionExpressionInvocation(
                    identifier(breakName), [])));
          });
          currentBlock = savedBlock;
          addStatement(AstFactory.assignmentExpression(
              identifier(continueNames[index]), scanner.TokenType.EQ,
              functionExpression([], caseBlock)));
          ++index;
        }
      }

      // Translate the unlabeled cases as blocks and the labeled cases as
      // calls to the corresponding continue function.
      var savedBlock = currentBlock;
      var members = [];
      var index = 0;
      for (var member in node.members) {
        var bodyBlock;
        if (member.labels.isEmpty) {
          bodyBlock = currentBlock = emptyBlock();
          _translateStatementList(member.statements, r, () {
            addStatement(AstFactory.returnStatement2(
                AstFactory.functionExpressionInvocation(
                    identifier(breakName), [])));
          });
        } else {
          bodyBlock = block([AstFactory.returnStatement2(
              AstFactory.functionExpressionInvocation(
                  identifier(continueNames[index]), []))]);
          ++index;
        }
        // Cases must end with return, break, continue, or throw.
        if (member is ast.SwitchDefault) {
          members.add(AstFactory.switchDefault2(bodyBlock.statements));
        } else {
          members.add(AstFactory.switchCase(member.expression,
              bodyBlock.statements));
        }
      }
      _removeJumpTargets(node, breakTargets, breakLabels);
      continueTargets.length = savedLength;
      continueTargets.length = savedLength;
      currentBlock = savedBlock;
      addStatement(AstFactory.switchStatement(expr, members));
    });
  };

  _translateCatchClause(ast.CatchClause node, r, s) {
    var savedBlock = currentBlock;
    var catchBlock = currentBlock = emptyBlock();
    visit(node.body)(r, s);
    currentBlock = savedBlock;
    var parameters = [node.exceptionParameter.name];
    if (node.stackTraceParameter != null) {
      parameters.add(node.stackTraceParameter.name);
    }
    var args = [functionExpression(parameters, catchBlock)];
    if (node.onKeyword != null) {
      // We do not need to worry about `e` shadowing anything.
      args.add(functionExpression(['e'],
          AstFactory.isExpression(identifier('e'), false, node.exceptionType)));
    }
    return args;
  }

  visitTryStatement(ast.TryStatement node) => (r, s) {
    ast.Expression newJumpTarget(ast.Expression target) {
      if (target is ast.FunctionExpressionInvocation) {
        // Eta reduce.  If the target is of the form expr(), then
        // () => expr() is the same as expr.
        return target.function;
      } else {
        return functionExpression([], target);
      }
    }

    var joinName = newName('join');
    ast.FunctionExpression joinFun = reifyStatementCont(s);
    addStatement(
        AstFactory.functionDeclarationStatement(null, null, joinName, joinFun));

    var savedBlock = currentBlock;
    var savedBreakTargets = breakTargets;
    var savedContinueTargets = continueTargets;
    var finallyName, finallyContName, finallyBlock;
    if (node.finallyBlock != null) {
      finallyName = newName('finally');
      finallyContName = newName('cont');
      finallyBlock = currentBlock = emptyBlock();
      visit(node.finallyBlock)(r, () {
        addStatement(AstFactory.returnStatement2(
            AstFactory.functionExpressionInvocation(
                identifier(finallyContName), [])));
      });

      breakTargets = breakTargets.map(newJumpTarget).toList();
      continueTargets = continueTargets.map(newJumpTarget).toList();
      var ret = r;
      r = (v) {
        addStatement(
            AstFactory.returnStatement2(
                functionExpression([], applyExpressionCont(ret, v))));
      };
      s = () {
        addStatement(
            AstFactory.returnStatement2(identifier(joinName)));
      };
    } else {
      s = () {
        addStatement(
            AstFactory.returnStatement2(
                AstFactory.functionExpressionInvocation(
                    identifier(joinName), [])));
      };
    }

    var catchErrorArgs = node.catchClauses.map(
        (c) => _translateCatchClause(c, r, s));

    var tryBlock = currentBlock = emptyBlock();
    visit(node.body)(r, s);

    currentBlock = savedBlock;
    breakTargets = savedBreakTargets;
    continueTargets = savedContinueTargets;
    if (finallyBlock != null) {
      addStatement(
          AstFactory.functionDeclarationStatement(
              null,
              null,
              finallyName,
              functionExpression([finallyContName], finallyBlock)));
    }

    var expr = AstFactory.instanceCreationExpression2(
        scanner.Keyword.NEW,
        AstFactory.typeName3(
            AstFactory.identifier5('Future', 'sync'), []),
        [functionExpression([], tryBlock)]);
    for (var args in catchErrorArgs) {
      expr = AstFactory.methodInvocation(expr, 'catchError', args);
    }
    if (finallyBlock != null) {
      var name = newName('e');
      expr = AstFactory.methodInvocation(
          expr,
          'then',
          [identifier(finallyName),
           AstFactory.namedExpression2('onError',
               functionExpression([name],
                   AstFactory.functionExpressionInvocation(
                       identifier(finallyName),
                       [functionExpression([],
                           AstFactory.throwExpression2(
                               identifier(name)))])))]);
    }
    addStatement(AstFactory.returnStatement2(expr));
  };

  _translateDeclarationList(scanner.Keyword keyword,
      ast.VariableDeclarationList node, s) {
    translateDecl(ast.VariableDeclaration decl, cont) {
      if (decl.initializer == null) {
        return cont(decl);
      } else {
        return visit(decl.initializer)((expr) {
          return cont(AstFactory.variableDeclaration2(decl.name.name, expr));
        });
      }
    }

    var decls = [];
    // The continuation for the last declaration.
    var cont = (decl) {
      decls.add(decl);
      return s(decls);
    };
    for (var i = node.variables.length - 1; i >= 1; --i) {
      var nextCont = cont;
      // The continuation for the i-1 declaration.
      cont = (decl) {
        decls.add(decl);
        var nextDecl = node.variables[i];
        if (awaits.contains(nextDecl)) {
          _residualizeDeclarationList(keyword, decls);
          decls.clear();
        }
        translateDecl(nextDecl, nextCont);
      };
    }
    translateDecl(node.variables.first, cont);
  }

  void _residualizeDeclarationList(scanner.Keyword keyword,
      List<ast.VariableDeclaration> decls) {
    if (decls.isEmpty) return;
    addStatement(AstFactory.variableDeclarationStatement2(keyword, decls));
  }

  visitVariableDeclarationStatement(
      ast.VariableDeclarationStatement node) => (r, s) {
    // TODO(kmillikin): A null keyword indicates a type.  Do not discard it!
    var keyword = node.variables.keyword == null
        ? scanner.Keyword.VAR
        : scanner.Keyword.keywords[node.variables.keyword.lexeme];
    return _translateDeclarationList(keyword, node.variables, (decls) {
      _residualizeDeclarationList(keyword, decls);
      return s();
    });
  };

  visitWhileStatement(ast.WhileStatement node) => (r, s) {
    var breakName = newName('break');
    var continueName = newName('continue');

    var savedBlock = currentBlock;
    var breakBlock = currentBlock = emptyBlock();
    s();

    var continueBlock = currentBlock = emptyBlock();
    visit(node.condition)((expr) {
      var savedBlock = currentBlock;
      var bodyBlock = currentBlock = emptyBlock();

      _addJumpTargets(node, breakName, breakTargets, breakLabels);
      _addJumpTargets(node, continueName, continueTargets, continueLabels);
      visit(node.body)(r, () {
        addStatement(
            AstFactory.returnStatement2(
                AstFactory.functionExpressionInvocation(
                    identifier(continueName), [])));
      });
      _removeJumpTargets(node, breakTargets, breakLabels);
      _removeJumpTargets(node, continueTargets, continueLabels);
      currentBlock = savedBlock;

      addStatement(AstFactory.ifStatement2(
        expr,
        // Trampoline the body via Future.wait.
        block([AstFactory.returnStatement2(AstFactory.methodInvocation(
            AstFactory.functionExpressionInvocation(
                AstFactory.identifier(identifier('Future'),
                                      identifier('wait')),
                [AstFactory.listLiteral([])]),
            'then',
            [functionExpression([newName('x')], bodyBlock)]))]),
        block([
            AstFactory.returnStatement2(
                AstFactory.functionExpressionInvocation(
                    identifier(breakName), []))])));
    });

    currentBlock = savedBlock;
    addStatement(
        AstFactory.functionDeclarationStatement(null, null, breakName,
            functionExpression([], breakBlock)));
    addStatement(
        AstFactory.functionDeclarationStatement(null, null, continueName,
            functionExpression([], continueBlock)));
    addStatement(
        AstFactory.returnStatement2(
            AstFactory.functionExpressionInvocation(
                identifier(continueName), [])));
  };

  visitYieldStatement(ast.YieldStatement node) => unimplemented(node);

  // ---- Expressions ----
  visitAsExpression(ast.AsExpression node) => (s) {
    return visit(node.expression)((expr) {
      return s(AstFactory.asExpression(expr, node.type));
    });
  };

  final Map _assignmentToBinaryOperator = {
      scanner.TokenType.AMPERSAND_EQ: scanner.TokenType.AMPERSAND,
      scanner.TokenType.BAR_EQ: scanner.TokenType.BAR,
      scanner.TokenType.CARET_EQ: scanner.TokenType.CARET,
      scanner.TokenType.GT_GT_EQ: scanner.TokenType.GT_GT,
      scanner.TokenType.LT_LT_EQ: scanner.TokenType.LT_LT,
      scanner.TokenType.MINUS_EQ: scanner.TokenType.MINUS,
      scanner.TokenType.PERCENT_EQ: scanner.TokenType.PERCENT,
      scanner.TokenType.PLUS_EQ: scanner.TokenType.PLUS,
      scanner.TokenType.SLASH_EQ: scanner.TokenType.SLASH,
      scanner.TokenType.STAR_EQ: scanner.TokenType.STAR,
      scanner.TokenType.TILDE_SLASH_EQ: scanner.TokenType.TILDE_SLASH,
    };

  visitAssignmentExpression(ast.AssignmentExpression node) => (s) {
    finishAssignment(lhs) {
      var nameLeft = node.operator.type != scanner.TokenType.EQ &&
          awaits.contains(node.rightHandSide);
      var lhsValue;
      if (nameLeft) lhsValue = addTempDeclaration(lhs);
      return visit(node.rightHandSide)((rhs) {
        if (nameLeft) {
          return s(AstFactory.assignmentExpression(lhs, scanner.TokenType.EQ,
              AstFactory.binaryExpression(lhsValue,
                  _assignmentToBinaryOperator[node.operator.type],
                  rhs)));
        } else {
          return s(AstFactory.assignmentExpression(lhs,
              node.operator.type, rhs));
        }
      });
    }

    var lhs = node.leftHandSide;
    if (lhs is ast.SimpleIdentifier) {
      return finishAssignment(lhs);
    } else if (lhs is ast.PrefixedIdentifier) {
      // TODO(kmillikin): We need resolution to determine what the prefix is.
      // It could be a class or library name, in which case we do not want
      // to rename it.  Or it could be a mutable variable in scope, in
      // which case we do.
      //
      // Translate this as if it were a property access, though that could
      // be wrong.
      var target = lhs.prefix;
      if (awaits.contains(node.rightHandSide)) {
        target = addTempDeclaration(target);
      }
      return finishAssignment(
          AstFactory.propertyAccess(target, lhs.identifier));
    } else if (lhs is ast.PropertyAccess) {
      if (lhs.target != null) {
        return visit(lhs.target)((target) {
          if (awaits.contains(node.rightHandSide)) {
            target = addTempDeclaration(target);
          }
          return finishAssignment(
              AstFactory.propertyAccess(target, lhs.propertyName));
        });
      } else {
        return finishAssignment(
            AstFactory.propertyAccess(null, lhs.propertyName));
      }
    } else if (lhs is ast.IndexExpression) {
      if (lhs.target != null) {
        return visit(lhs.target)((target) {
          if (awaits.contains(lhs.index) ||
              awaits.contains(node.rightHandSide)) {
            target = addTempDeclaration(target);
          }
          return visit(lhs.index)((index) {
            if (awaits.contains(node.rightHandSide)) {
              index = addTempDeclaration(index);
            }
            return finishAssignment(AstFactory.indexExpression(target, index));
         });
        });
      } else {
        return visit(lhs.index)((index) {
          if (awaits.contains(node.rightHandSide)) {
            index = addTempDeclaration(index);
          }
          return finishAssignment(AstFactory.indexExpression(null, index));
        });
      }
    } else {
      throw  'Unexpected ${lhs.runtimeType} in assignment: $lhs';
    }
  };

  visitAwaitExpression(ast.AwaitExpression node) => (s) {
    visit(node.expression)((expr) {
      addStatement(AstFactory.returnStatement2(
          AstFactory.methodInvocation(
                            expr,
                            'then',
                            [reifyExpressionCont(s, 'x')])));
    });
  };

  visitBinaryExpression(ast.BinaryExpression node) => (s) {
    if (node.operator.lexeme == '&&' || node.operator.lexeme == '||') {
      if (awaits.contains(node.rightOperand)) {
        var joinName = newName('join');
        addStatement(AstFactory.functionDeclarationStatement(null,  null,
            joinName, reifyExpressionCont(s, 'x')));
        visit(node.leftOperand)((left) {
          var savedBlock = currentBlock;
          var rightBlock = currentBlock = emptyBlock();
          visit(node.rightOperand)((right) {
            addStatement(AstFactory.functionExpressionInvocation(
                identifier(joinName), [
                    AstFactory.conditionalExpression(right,
                        AstFactory.booleanLiteral(true),
                        AstFactory.booleanLiteral(false))]));
          });
          currentBlock = savedBlock;
          if (node.operator.lexeme == '&&') {
            addStatement(AstFactory.ifStatement2(
                left,
                rightBlock,
                block([AstFactory.functionExpressionInvocation(
                    identifier(joinName),
                    [AstFactory.booleanLiteral(false)])])));
          } else {
            addStatement(AstFactory.ifStatement2(
                left,
                block([AstFactory.functionExpressionInvocation(
                    identifier(joinName),
                    [AstFactory.booleanLiteral(true)])]),
                rightBlock));
          }
        });
      } else {
        visit(node.leftOperand)((left) {
          visit(node.rightOperand)((right) {
            s(AstFactory.binaryExpression(left, node.operator.type, right));
          });
        });
      }
    } else {
      return visit(node.leftOperand)((left) {
        if (awaits.contains(node.rightOperand)) {
          left = addTempDeclaration(left);
        }
       return visit(node.rightOperand)((right) {
          s(AstFactory.binaryExpression(left, node.operator.type, right));
        });
      });
    }
  };

  ast.Expression _asCascadeSection(ast.Expression node) {
    if (node is ast.IndexExpression) {
      return AstFactory.cascadedIndexExpression(node.index);
    } else if (node is ast.MethodInvocation) {
      return AstFactory.cascadedMethodInvocation(node.methodName.name,
          node.argumentList.arguments);
    } else if (node is ast.PropertyAccess) {
      return AstFactory.cascadedPropertyAccess(node.propertyName.name);
    } else if (node is ast.AssignmentExpression) {
      return AstFactory.assignmentExpression(
          _asCascadeSection(node.leftHandSide), node.operator.type,
          node.rightHandSide);
    }
    return unreachable(node);
  }

  visitCascadeExpression(ast.CascadeExpression node) => (s) {
    visit(node.target)((target) {
      if (node.cascadeSections.any(awaits.contains)) {
        target = addTempDeclaration(target);
      }
      var sections = [];
      var cont = (e) {
        sections.add(_asCascadeSection(e));
        s(AstFactory.cascadeExpression(target, sections));
      };
      for (var i = node.cascadeSections.length - 1; i >= 1; --i) {
        var nextCont = cont;
        // The continuation for the i-1 cascade section.
        cont = (e) {
          sections.add(_asCascadeSection(e));
          var nextSection = node.cascadeSections[i];
          if (awaits.contains(nextSection) && sections.isNotEmpty) {
            addStatement(AstFactory.cascadeExpression(target, sections));
            sections.clear();
          }
          visit(nextSection)(nextCont);
        };
      }
      return visit(node.cascadeSections.first)(cont);
    });
  };

  visitConditionalExpression(ast.ConditionalExpression node) => (s) {
    return visit(node.condition)((expr) {
      var joinName = newName('join');
      addStatement(
          AstFactory.functionDeclarationStatement(
              null, null, joinName, reifyExpressionCont(s, 'x')));
      s = (r) {
        addStatement(AstFactory.methodInvocation2(joinName, [r]));
      };
      var savedBlock = currentBlock;
      var thenBlock = currentBlock = emptyBlock();
      visit(node.thenExpression)(s);
      var elseBlock = currentBlock = emptyBlock();
      visit(node.elseExpression)(s);
      currentBlock = savedBlock;
      addStatement(AstFactory.ifStatement2(expr, thenBlock, elseBlock));
    });
  };

  visitFunctionExpression(ast.FunctionExpression node) => (s) {
    node.body = new AsyncTransformer().visit(node.body);
    return s(AstFactory.parenthesizedExpression(node));
  };

  visitFunctionExpressionInvocation(
      ast.FunctionExpressionInvocation node) => (s) {
    return visit(node.function)((rator) {
      if (awaits.contains(node.argumentList)) {
        rator = addTempDeclaration(rator);
      }
      return _translateExpressionList(node.argumentList.arguments, (rands) {
        return s(AstFactory.functionExpressionInvocation(rator, rands));
      });
    });
  };

  // ---- Identifiers ----
  visitSimpleIdentifier(ast.SimpleIdentifier node) => (s) {
    return s(node);
  };

  visitPrefixedIdentifier(ast.PrefixedIdentifier node) => (s) {
    return s(node);
  };

  visitIndexExpression(ast.IndexExpression node) => (s) {
    return visit(node.target)((e0) {
      if (awaits.contains(node.index)) {
        e0 = addTempDeclaration(e0);
      }
      return visit(node.index)((e1) {
        s(AstFactory.indexExpression(e0, e1));
      });
    });
  };

  _translateExpressionList(ast.NodeList<ast.Expression> exprs, s) {
    if (exprs.isEmpty) {
      return s([]);
    }
    var args = [];
    var seenAwait = false;
    var cont = (v) {
      args.add(v);
      return s(args);
    };
    for (var i = exprs.length - 1; i >= 1; --i) {
      var expr = exprs[i];
      seenAwait = seenAwait || awaits.contains(expr);
      var current = cont;
      if (seenAwait) {
        cont = (v) {
          var value = addTempDeclaration(v);
          args.add(value);
          visit(expr)(current);
        };
      } else {
        cont = (v) {
          args.add(v);
          visit(expr)(current);
        };
      }
    }
    return visit(exprs.first)(cont);
  }

  visitInstanceCreationExpression(
      ast.InstanceCreationExpression node) => (s) {
    _translateExpressionList(node.argumentList.arguments, (rands) {
      s(AstFactory.instanceCreationExpression(
              scanner.Keyword.keywords[node.keyword.lexeme],
              node.constructorName,
              rands));
    });
  };

  visitIsExpression(ast.IsExpression node) => (s) {
    return visit(node.expression)((expr) {
      return s(AstFactory.isExpression(
          expr, node.notOperator != null, node.type));
    });
  };

  // ---- Literals ----
  visitBooleanLiteral(ast.BooleanLiteral node) => (s) {
    s(node);
  };

  visitDoubleLiteral(ast.DoubleLiteral node) => (s) {
    s(node);
  };

  visitIntegerLiteral(ast.IntegerLiteral node) => (s) {
    s(node);
  };

  visitNullLiteral(ast.NullLiteral node) => (s) {
    return s(node);
  };

  // ---- StringLiterals ----
  visitAdjacentStrings(ast.AdjacentStrings node) => (s) {
    assert(node.strings.isNotEmpty);
    // This code relies on translating a StringLiteral never passing a
    // non-StringLiteral to its success continuation.
    var strings = [];
    var cont = (e) {
      strings.add(e);
      s(AstFactory.adjacentStrings(strings));
    };
    for (var i = node.strings.length - 1; i >= 1; --i) {
      // Build the continuation for the i-1 string.
      var nextCont = cont;
      cont = (e) {
        strings.add(e);
        visit(node.strings[i])(nextCont);
      };
    }
    return visit(node.strings.first)(cont);
  };

  visitSimpleStringLiteral(ast.SimpleStringLiteral node) => (s) {
    // The translation of adjacent strings relies on never passing a
    // non-StringLiteral to the success continuation here.
    return s(node);
  };

  visitStringInterpolation(ast.StringInterpolation node) => (s) {
    var list = new ast.NodeList<ast.Expression>(node);
    for (var element in node.elements) {
      if (element is ast.InterpolationExpression) {
        list.add(element.expression);
      } else {
        assert(element is ast.InterpolationString);
      }
    }
    _translateExpressionList(list, (exprs) {
      var elements = <ast.InterpolationElement>[];
      int index = 0;
      for (var element in node.elements) {
        if (element is ast.InterpolationExpression) {
          elements.add(AstFactory.interpolationExpression(exprs[index++]));
        } else {
          elements.add(element);
        }
      }
      // The translation of adjacent strings relies on never passing a
      // non-StringLiteral to the success continuation here.
      s(AstFactory.string(elements));
    });
  };

  visitSymbolLiteral(ast.SymbolLiteral node) => (s) {
    return s(node);
  };

  // ---- TypedLiterals ----
  visitListLiteral(ast.ListLiteral node) => (s) {
    _translateExpressionList(node.elements, (elts) {
      s(AstFactory.listLiteral(elts));
    });
  };

  visitMapLiteral(ast.MapLiteral node) => (s) {
    var list = new ast.NodeList<ast.Expression>(node);
    for (var entry in node.entries) {
      list.add(entry.key);
      list.add(entry.value);
    }
    _translateExpressionList(list, (exprs) {
      var entries = <ast.MapLiteralEntry>[];
      for (var i = 0; i < exprs.length; i += 2) {
        entries.add(new ast.MapLiteralEntry(
                exprs[i],
                TokenFactory.tokenFromType(scanner.TokenType.COLON),
                exprs[i + 1]));
      }
      s(AstFactory.mapLiteral2(entries));
    });
  };

  visitMethodInvocation(ast.MethodInvocation node) => (s) {
    if (node.target != null) {
      visit(node.target)((rator) {
        if (awaits.contains(node.argumentList)) {
          rator = addTempDeclaration(rator);
        }
        _translateExpressionList(node.argumentList.arguments, (rands) {
          s(AstFactory.methodInvocation(rator, node.methodName.name, rands));
        });
      });
    } else {
      _translateExpressionList(node.argumentList.arguments, (rands) {
        s(AstFactory.methodInvocation2(node.methodName.name, rands));
      });
    }
  };

  visitNamedExpression(ast.NamedExpression node) => (s) {
    return visit(node.expression)((expr) {
      return s(AstFactory.namedExpression(node.name, expr));
    });
  };

  visitParenthesizedExpression(ast.ParenthesizedExpression node) => (s) {
    return visit(node.expression)(s);
  };

  visitPostfixExpression(ast.PostfixExpression node) => (s) {
    return visit(node.operand)((expr) {
      return s(AstFactory.postfixExpression(expr, node.operator.type));
    });
  };

  visitPrefixExpression(ast.PrefixExpression node) => (s) {
    return visit(node.operand)((expr) {
      return s(AstFactory.prefixExpression(node.operator.type, expr));
    });
  };

  visitPropertyAccess(ast.PropertyAccess node) => (s) {
    return visit(node.target)((expr) {
      return s(AstFactory.propertyAccess(expr, node.propertyName));
    });
  };

  visitSuperExpression(ast.SuperExpression node) => (s) {
    return s(node);
  };

  visitThisExpression(ast.ThisExpression node) => (s) {
    return s(node);
  };

  visitThrowExpression(ast.ThrowExpression node) => (s) {
    return visit(node.expression)((expr) {
      return s(AstFactory.throwExpression2(expr));
    });
  };

  unreachable(node) => throw 'Unreachable(${node.runtimeType})';
  visitAnnotation(node) => unreachable(node);
  visitArgumentList(node) => unreachable(node);
  visitCatchClause(node) => unreachable(node);
  visitComment(node) => unreachable(node);
  visitCommentReference(node) => unreachable(node);
  visitConstructorFieldInitializer(node) => unreachable(node);
  visitConstructorName(node) => unreachable(node);
  visitDeclaredIdentifier(node) => unreachable(node);
  visitDefaultFormalParameter(node) => unreachable(node);
  visitEnumConstantDeclaration(node) => unreachable(node);
  visitExportDirective(node) => unreachable(node);
  visitExtendsClause(node) => unreachable(node);
  visitFieldFormalParameter(node) => unreachable(node);
  visitFormalParameterList(node) => unreachable(node);
  visitFunctionTypedFormalParameter(node) => unreachable(node);
  visitHideCombinator(node) => unreachable(node);
  visitImplementsClause(node) => unreachable(node);
  visitImportDirective(node) => unreachable(node);
  visitInterpolationExpression(node) => unreachable(node);
  visitInterpolationString(node) => unreachable(node);
  visitLabel(node) => unreachable(node);
  visitLibraryDirective(node) => unreachable(node);
  visitLibraryIdentifier(node) => unreachable(node);
  visitMapLiteralEntry(node) => unreachable(node);
  visitNativeClause(node) => unreachable(node);
  visitPartDirective(node) => unreachable(node);
  visitPartOfDirective(node) => unreachable(node);
  visitRedirectingConstructorInvocation(node) => unreachable(node);
  visitScriptTag(node) => unreachable(node);
  visitShowCombinator(node) => unreachable(node);
  visitSimpleFormalParameter(node) => unreachable(node);
  visitSuperConstructorInvocation(node) => unreachable(node);
  visitSwitchCase(node) => unreachable(node);
  visitSwitchDefault(node) => unreachable(node);
  visitTypeArgumentList(node) => unreachable(node);
  visitTypeName(node) => unreachable(node);
  visitTypeParameter(node) => unreachable(node);
  visitTypeParameterList(node) => unreachable(node);
  visitVariableDeclaration(node) => unreachable(node);
  visitVariableDeclarationList(node) => unreachable(node);
  visitWithClause(node) => unreachable(node);
}

class SyncTransformer extends ast.RecursiveAstVisitor {
  visit(ast.AstNode node) => node.accept(this);

  visitFunctionExpression(ast.FunctionExpression node) {
    if (node.body.isGenerator) {
      throw 'Transfomer: unsupported generator function.';
    }
    visit(node.parameters);
    if (node.body.isAsynchronous) {
      node.body = new AsyncTransformer().visit(node.body);
    } else {
      visit(node.body);
    }
  }
}
