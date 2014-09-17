// Copyright (c) 2014, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

library async_await.src.xform;

import 'package:analyzer/src/generated/ast.dart' as ast;
import 'package:analyzer/src/generated/scanner.dart' as scanner;
import 'package:analyzer/src/generated/testing/token_factory.dart';

import 'package:async_await/src/ast_factory.dart' as make;

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

ast.VariableDeclarationStatement variableDeclaration(
    String name, ast.Expression initializer) {
  return make.variableDeclarationStatement(scanner.Keyword.FINAL,
          [make.variableDeclaration(name, initializer)]);
}

// Information needed to generate a break or continue from a loop or labeled
// statement.  Needed is:
//
// * A list of labels (statements can have multiple labels), used to
//   search for the target given a label of a break or continue.
// * A zero-argument function to apply to generate the code for the jump.
//   It returns null and has the side effect of adding the code for the
//   jump to the curent block.
// * An optional loop trampoline flag.  When breaking out of loops the
//   trampoline flag for the loop must be set to false.  The apply function
//   sets the flag, but `await` does not call the apply function and sets the
//   flags manually instead (await returns immediately without passing through
//   finally blocks, for instance).
class JumpTarget {
  final List<ast.Label> labels;
  final apply;
  final String inLoopName;

  JumpTarget(this.labels, this.apply, [this.inLoopName]);
}

class AsyncTransformer extends ast.AstVisitor {
  Set<ast.AstNode> awaits;
  Map<ast.Statement, List<ast.Label>> labels;
  Set<String> names;

  ast.Block currentBlock;

  // Stacks of targets for break and continue.
  List<JumpTarget> breakTargets;
  List<JumpTarget> continueTargets;

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
    currentBlock = make.emptyBlock();
    breakTargets = <JumpTarget>[];
    continueTargets = <JumpTarget>[];
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
    return make.identifier(name);
  }

  void addStatement(ast.AstNode node) {
    if (node is ast.Statement) {
      currentBlock.statements.add(node);
    } else {
      assert(node is ast.Expression);
      currentBlock.statements.add(make.expressionStatement(node));
    }
  }

  ast.FunctionExpression reifyExpressionCont(f, baseName) {
    var savedBlock = currentBlock;
    var bodyBlock = currentBlock = make.emptyBlock();
    String name = newName(baseName);
    f(make.identifier(name));
    currentBlock = savedBlock;
    return make.functionExpression([name], bodyBlock);
  }

  ast.AstNode applyExpressionCont(f, v) {
    v = addTempDeclaration(v);
    var savedBlock = currentBlock;
    var bodyBlock = currentBlock = make.emptyBlock();
    f(v);
    currentBlock = savedBlock;

    var body = bodyBlock;
    if (body.statements.length == 1 &&
        body.statements.first is ast.ReturnStatement) {
      body = (body.statements.first as ast.ReturnStatement).expression;
      if (body == null) body = make.nullLiteral();
    }
    return body;
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
      addStatement(make.returnStatement(v));
    }, () {
      addStatement(make.returnStatement());
    });

    return make.functionBody(
        make.returnStatement(make.newInstance(
            make.identifier('Future', 'microtask'),
            [make.functionExpression([], currentBlock)])));
  }

  visitEmptyFunctionBody(ast.EmptyFunctionBody node) {
    return node;
  }

  visitExpressionFunctionBody(ast.ExpressionFunctionBody node) {
    var body = make.functionBody(make.returnStatement(node.expression));
    (body as ast.BlockFunctionBody).keyword = node.keyword;
    return visit(body);
  }

  visitNativeFunctionBody(ast.NativeFunctionBody node) {
    return node;
  }

  // ---- Statements ----
  unimplemented(ast.AstNode node) => throw 'Unimplemented(${node.runtimeType})';

  visitAssertStatement(ast.AssertStatement node) => (r, s) {
    return visit(node.condition)((cond) {
      addStatement(make.assertStatement(cond));
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

  _newJumpTarget(String inLoopName) {
    return (t) {
      if (inLoopName == null) return t;
      return new JumpTarget(t.labels, () {
        addStatement(make.assignmentExpression(
            make.identifier(inLoopName), make.booleanLiteral(false)));
        return t.apply();
      }, t.inLoopName);
    };
  }

  List<JumpTarget> _addBreakTarget(List<ast.Label> labels, String targetName,
      [String inLoopName]) {
    var result = new List.from(breakTargets)..add(
        new JumpTarget(labels, () {
          addStatement(
              make.returnStatement(make.functionInvocation(targetName)));
        }, inLoopName));
    return result.map(_newJumpTarget(inLoopName)).toList();
  }

  List<JumpTarget> _addContinueTarget(List<ast.Label> labels,
      String targetName, [String inLoopName]) {
    var result = continueTargets.map(_newJumpTarget(inLoopName)).toList();
    return result..add(new JumpTarget(labels, () {
      addStatement(
          make.returnStatement(make.functionInvocation(targetName)));
    }, inLoopName));
  }

  JumpTarget _findJumpTarget(node, List<JumpTarget> targets) {
    if (node.label == null) {
      return targets.last;
    }
    for (var target in targets.reversed) {
      if (target.labels == null) continue;
      for (var label in target.labels) {
        if (label.label.name == node.label.name) return target;
      }
    }
    throw "Could not find target for '$node'";
  }

  visitBreakStatement(ast.BreakStatement node) => (r, s) {
    var target = _findJumpTarget(node, breakTargets);
    return target.apply();
  };

  visitContinueStatement(ast.ContinueStatement node) => (r, s) {
    var target = _findJumpTarget(node, continueTargets);
    return target.apply();
  };

  // The template of the loop trampoline loop.  A trampoline is used to avoid
  // stack overflow for loops that contain await but have a synchronous path
  // through the loop.
  //
  // If 'bounce' is the function containing the translation of the loop's
  // continue target (which is different for do/while, for, and while loops)
  // then the actual continue target is the trampoline loop:
  //
  // continue_ = () {
  //     if (inLoop) return null;
  //     inLoop = true;
  //     var result;
  //     while (inLoop) result = bounce();
  //     return result;
  // };
  void _addTrampoline(String bounceName, String continueName,
                      String inLoopName) {
    var resultName = newName('result');
    addStatement(
        make.assignmentExpression(make.identifier(continueName),
            make.functionExpression([], make.block(
                [make.ifStatement(make.identifier(inLoopName),
                     make.returnStatement()),
                 make.assignmentExpression(
                     make.identifier(inLoopName), make.booleanLiteral(true)),
                 make.variableDeclarationStatement(scanner.Keyword.VAR,
                     [make.variableDeclaration(resultName)]),
                 make.whileStatement(make.identifier(inLoopName),
                     make.expressionStatement(
                         make.assignmentExpression(
                             make.identifier(resultName),
                             make.functionInvocation(bounceName)))),
                 make.returnStatement(make.identifier(resultName))]))));
  }

  // [[do S1 while (E); S2]] =
  //     var inLoop = false;
  //     break_() {
  //         [[S2]]
  //     }
  //     loop() {
  //         var continue_;
  //         bounce() {
  //             final v = [[E]];
  //             if (v) {
  //                 return loop();
  //             } else {
  //                 inLoop = false;
  //                 return break_();
  //             }
  //         }
  //         continue_ = () {
  //             if (inLoop) return null;
  //             inLoop = true;
  //             var result;
  //             while (inLoop) result = bounce();
  //             return result;
  //         };
  //         [[S1]]
  //         return continue_();
  //     }
  //     return loop();
  visitDoStatement(ast.DoStatement node) => (r, s) {
    var breakName = newName('break');
    var continueName = newName('continue');
    var inLoopName = newName('inLoop');
    var bounceName = newName('bounce');
    var loopName = newName('loop');

    var savedBlock = currentBlock;
    var breakBlock = currentBlock = make.emptyBlock();
    s();

    // Though break and continue cannot occur in the loop condition, the
    // targets are added here.  An await in the condition is like a break so
    // the break target should be in scope for the condition.  Continue is
    // set so that the two stacks of targets are handled uniformly.
    var savedBreakTargets = breakTargets;
    var savedContinueTargets = continueTargets;
    breakTargets = _addBreakTarget(labels[node], breakName, inLoopName);
    continueTargets =
        _addContinueTarget(labels[node], continueName, inLoopName);
    var bounceBlock = currentBlock = make.emptyBlock();
    visit(node.condition)((expr) {
      addStatement(make.ifStatement(
          expr,
          make.block(
              [make.returnStatement(make.functionInvocation(loopName))]),
          make.block(
              [make.assignmentExpression(
                   make.identifier(inLoopName), make.booleanLiteral(false)),
               make.returnStatement(make.functionInvocation(breakName))])));
    });

    var loopBlock = currentBlock = make.emptyBlock();
    addStatement(
        make.variableDeclarationStatement(scanner.Keyword.VAR,
            [make.variableDeclaration(continueName)]));
    addStatement(
        make.functionDeclarationStatement(bounceName, [], bounceBlock));
    _addTrampoline(bounceName, continueName, inLoopName);
    visit(node.body)((v) {
      addStatement(make.assignmentExpression(
          make.identifier(inLoopName), make.booleanLiteral(false)));
      return r(v);
    }, () {
      addStatement(make.returnStatement(make.functionInvocation(continueName)));
    });

    breakTargets = savedBreakTargets;
    continueTargets = savedContinueTargets;
    currentBlock = savedBlock;
    addStatement(
        make.variableDeclarationStatement(scanner.Keyword.VAR,
            [make.variableDeclaration(inLoopName,
                 make.booleanLiteral(false))]));
    addStatement(
        make.functionDeclarationStatement(breakName, [], breakBlock));
    addStatement(
        make.functionDeclarationStatement(loopName, [], loopBlock));
    addStatement(make.returnStatement(make.functionInvocation(loopName)));
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
    var stmt = make.emptyBlock();
    var it = newName('it');
    stmt.statements.add(
        make.variableDeclarationStatement(
            scanner.Keyword.VAR,
            [make.variableDeclaration(it,
                 make.propertyAccess(node.iterator, 'iterator'))]));
    var body;
    if (node.identifier != null) {
      body = make.block(
          [make.assignmentExpression(
               node.identifier,
               make.propertyAccess(make.identifier(it), 'current')),
           node.body]);
    } else {
      assert(node.loopVariable != null);
      body = make.block(
          [make.variableDeclarationStatement(
              scanner.Keyword.keywords[node.loopVariable.keyword.lexeme],
              [make.variableDeclaration(
                   node.loopVariable.identifier.name,
                   make.propertyAccess(make.identifier(it), 'current'))]),
           node.body]);
    }
    stmt.statements.add(make.whileStatement(
        make.methodInvocation(make.identifier(it), 'moveNext', []),
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
            ? nextCont(make.nullLiteral())
            : visit(nextExpr)(nextCont);
      };
    }
    var expr = decls.first.initializer;
    return (expr == null)
        ? cont(make.nullLiteral())
        :  visit(expr)(cont);
  }

  // The intializer and update parts are not really expressions and all of them
  // are optional.  Informally though, the translation is:
  // [[for (E1; E2; E3) S1; S2]] =
  //     var inLoop = false;
  //     break_() {
  //         inLoop = false;
  //         [[S2]]
  //     }
  //     loop(x, ...) {
  //         var continue_;
  //         bounce() {
  //             [[E3]];
  //             return loop(x, ...);
  //         }
  //         continue_ = () {
  //             if (inLoop) return null;
  //             inLoop = true;
  //             var result;
  //             while (inLoop) result = bounce();
  //             return result;
  //         };
  //         final v = [[E2]];
  //         if (v) {
  //           [[S1]]
  //           return continue_();
  //         } else {
  //           inLoop = false;
  //           return break_();
  //         }
  //     }
  //     [[E1]];
  //     return loop();
  visitForStatement(ast.ForStatement node) => (r, s) {
    var breakName = newName('break');
    var continueName = newName('continue');
    var inLoopName = newName('inLoop');
    var bounceName = newName('bounce');
    var loopName = newName('loop');

    var savedBlock = currentBlock;
    var breakBlock = currentBlock = make.emptyBlock();
    s();

    var parameters;
    if (node.variables != null) {
      parameters = node.variables.variables.map((d) => d.name).toList();
    } else {
      parameters = <ast.SimpleIdentifier>[];
    }

    // Though break and continue cannot occur in the loop condition or
    // updaters, the targets are added here.  An await in the condition or
    // updaters is like a break so the break target should be in scope for
    // them.  Continue is set so that the two stacks of targets are handled
    // uniformly.
    var savedBreakTargets = breakTargets;
    var savedContinueTargets = continueTargets;
    breakTargets = _addBreakTarget(labels[node], breakName, inLoopName);
    continueTargets =
        _addContinueTarget(labels[node], continueName, inLoopName);
    var bounceBlock = currentBlock = make.emptyBlock();
    invokeLoop() {
      addStatement(
          make.returnStatement(make.functionInvocation(loopName, parameters)));
    }
    if (node.updaters != null) {
      _translateForUpdaters(node.updaters, invokeLoop);
    } else {
      invokeLoop();
    }

    var bodyBlock = currentBlock = make.emptyBlock();
    visit(node.body)((v) {
      addStatement(make.assignmentExpression(
          make.identifier(inLoopName), make.booleanLiteral(false)));
      return r(v);
    }, () {
      addStatement(make.returnStatement(make.functionInvocation(continueName)));
    });

    var loopBlock = currentBlock = make.emptyBlock();
    addStatement(
        make.variableDeclarationStatement(scanner.Keyword.VAR,
            [make.variableDeclaration(continueName)]));
    addStatement(
        make.functionDeclarationStatement(bounceName, [], bounceBlock));
    _addTrampoline(bounceName, continueName, inLoopName);
    if (node.condition != null) {
      visit(node.condition)((expr) {
        addStatement(make.ifStatement(
          expr,
          bodyBlock,
          make.block(
              [make.assignmentExpression(
                   make.identifier(breakName), make.booleanLiteral(false)),
               make.returnStatement(make.functionInvocation(breakName))])));
      });
    } else {
      addStatement(bodyBlock);
    }

    breakTargets = savedBreakTargets;
    continueTargets = savedContinueTargets;
    currentBlock = savedBlock;
    addStatement(
        make.variableDeclarationStatement(scanner.Keyword.VAR,
            [make.variableDeclaration(inLoopName,
                 make.booleanLiteral(false))]));
    addStatement(
        make.functionDeclarationStatement(breakName, [], breakBlock));
    addStatement(make.functionDeclarationStatement(loopName,
        parameters.map((e) => e.name).toList(), loopBlock));
    if (node.variables != null) {
      assert(node.variables.variables.isNotEmpty);
      return _translateForDeclarations(node.variables.variables, (args) {
        assert(args.length == parameters.length);
        addStatement(
            make.returnStatement(make.functionInvocation(loopName, args)));
      });
    } else if (node.initialization != null) {
      assert(parameters.isEmpty);
      return visit(node.initialization)((expr) {
        addStatement(expr);
        addStatement(make.returnStatement(make.functionInvocation(loopName)));
      });
    } else {
      assert(parameters.isEmpty);
      addStatement(make.returnStatement(make.functionInvocation(loopName)));
    }
  };

  visitFunctionDeclarationStatement(
      ast.FunctionDeclarationStatement node) => (r, s) {
    var decl = new AsyncTransformer().visit(node.functionDeclaration);
    addStatement(new ast.FunctionDeclarationStatement(decl));
    return s();
  };

  visitIfStatement(ast.IfStatement node) => (r, s) {
    return visit(node.condition)((expr) {
      var savedBlock = currentBlock;
      var joinName = newName('join');
      var joinBlock = currentBlock = make.emptyBlock();
      s();

      s = () {
        addStatement(make.returnStatement(make.functionInvocation(joinName)));
      };
      var thenBlock = currentBlock = make.emptyBlock();
      visit(node.thenStatement)(r, s);
      var elseBlock = currentBlock = make.emptyBlock();
      if (node.elseStatement != null) {
        visit(node.elseStatement)(r, s);
      } else {
        s();
      }

      currentBlock = savedBlock;
      addStatement(make.functionDeclarationStatement(joinName, [], joinBlock));
      addStatement(make.ifStatement(expr, thenBlock, elseBlock));
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
      var breakBlock = currentBlock = make.emptyBlock();
      s();
      currentBlock = savedBlock;

      addStatement(
          make.functionDeclarationStatement(breakName, [], breakBlock));

      var savedBreakTargets = breakTargets;
      breakTargets = _addBreakTarget(labels[stmt], breakName);
      visit(stmt)(r, () {
        addStatement(make.returnStatement(make.functionInvocation(breakName)));
      });
      breakTargets = savedBreakTargets;
    };
  }

  visitRethrowExpression(ast.RethrowExpression node) => unimplemented(node);

  visitReturnStatement(ast.ReturnStatement node) => (r, s) {
    return (node.expression == null)
        ? r(make.nullLiteral())
        : visit(node.expression)((v) => r(v));
  };

  visitSwitchStatement(ast.SwitchStatement node) => (r, s) {
    return visit(node.expression)((expr) {
      var savedBlock = currentBlock;
      var breakBlock = currentBlock = make.emptyBlock();
      s();

      currentBlock = savedBlock;
      var breakName = newName('break');
      addStatement(
          make.functionDeclarationStatement(breakName, [], breakBlock));

      // Generate a name per labeled case.  Since this is not a loop continue
      // there is no need to worry about inLoop flags.  To avoid reallocating
      // the continueTarget list in a loop, the targets are added manually
      // instead of using _addContinueTarget.
      var savedContinueTargets = continueTargets;
      continueTargets = new List.from(continueTargets);
      var continueNames = [];
      for (var member in node.members) {
        var labels = member.labels;
        if (labels.isNotEmpty) {
          var continueName = newName('continue');
          continueNames.add(continueName);
          continueTargets.add(new JumpTarget(member.labels, () {
            addStatement(make.returnStatement(
                make.functionInvocation(continueName)));
          }));
        }
      }

      // Translate the cases with bindings for the break and possible
      // continues.
      var savedBreakTargets = breakTargets;
      breakTargets = _addBreakTarget(labels[node], breakName);
      if (continueNames.isNotEmpty) {
        // Add declarations for mutable continue functions.
        addStatement(
            make.variableDeclarationStatement(scanner.Keyword.VAR,
                continueNames.map(make.variableDeclaration).toList()));

        // Translate the labeled cases as recursive functions.
        var index = 0;
        for (var member in node.members) {
          if (member.labels.isEmpty) continue;
          var savedBlock = currentBlock;
          var caseBlock = currentBlock = make.emptyBlock();
          _translateStatementList(member.statements, r, () {
            addStatement(
                make.returnStatement(make.functionInvocation(breakName)));
          });
          currentBlock = savedBlock;
          addStatement(make.assignmentExpression(
              make.identifier(continueNames[index]),
              make.functionExpression([], caseBlock)));
          ++index;
        }
      }

      // Translate the unlabeled cases as blocks and the labeled cases as
      // calls to the corresponding continue function.
      savedBlock = currentBlock;
      var members = [];
      var index = 0;
      for (var member in node.members) {
        var bodyBlock;
        if (member.labels.isEmpty) {
          bodyBlock = currentBlock = make.emptyBlock();
          _translateStatementList(member.statements, r, () {
            addStatement(
                make.returnStatement(make.functionInvocation(breakName)));
          });
        } else {
          bodyBlock = make.block([make.returnStatement(
              make.functionInvocation(continueNames[index]))]);
          ++index;
        }
        if (member is ast.SwitchDefault) {
          members.add(make.switchCase(null, bodyBlock.statements));
        } else {
          members.add(make.switchCase(member.expression, bodyBlock.statements));
        }
      }
      breakTargets = savedBreakTargets;
      continueTargets = savedContinueTargets;
      currentBlock = savedBlock;
      addStatement(make.switchStatement(expr, members));
    });
  };

  _translateCatchClause(ast.CatchClause node, r, s) {
    var savedBlock = currentBlock;
    var catchBlock = currentBlock = make.emptyBlock();
    visit(node.body)(r, s);
    currentBlock = savedBlock;
    var parameters = [node.exceptionParameter.name];
    if (node.stackTraceParameter != null) {
      parameters.add(node.stackTraceParameter.name);
    }
    var args = [make.functionExpression(parameters, catchBlock)];
    if (node.onKeyword != null) {
      // We do not need to worry about `e` shadowing anything.
      args.add(make.functionExpression(['e'],
          make.isExpression(make.identifier('e'), false, node.exceptionType)));
    }
    return args;
  }

  visitTryStatement(ast.TryStatement node) => (r, s) {
    JumpTarget newJumpTarget(JumpTarget target) {
      return new JumpTarget(target.labels, () {
        var savedBlock = currentBlock;
        var bodyBlock = currentBlock = make.emptyBlock();
        target.apply();
        currentBlock = savedBlock;
        addStatement(make.returnStatement(
            make.functionExpression([], bodyBlock)));
      });
    }

    var savedBlock = currentBlock;
    var joinName = newName('join');
    var joinBlock = currentBlock = make.emptyBlock();
    s();

    var savedBreakTargets = breakTargets;
    var savedContinueTargets = continueTargets;
    var finallyName, finallyContName, finallyBlock;
    if (node.finallyBlock != null) {
      finallyName = newName('finally');
      finallyContName = newName('cont');
      finallyBlock = currentBlock = make.emptyBlock();
      visit(node.finallyBlock)(r, () {
        addStatement(
            make.returnStatement(make.functionInvocation(finallyContName)));
      });

      breakTargets = breakTargets.map(newJumpTarget).toList();
      continueTargets = continueTargets.map(newJumpTarget).toList();
      var ret = r;
      r = (v) {
        addStatement(make.returnStatement(
            make.functionExpression([], applyExpressionCont(ret, v))));
      };
      s = () {
        addStatement(make.returnStatement(make.identifier(joinName)));
      };
    } else {
      s = () {
        addStatement(make.returnStatement(make.functionInvocation(joinName)));
      };
    }

    var catchErrorArgs = node.catchClauses.map(
        (c) => _translateCatchClause(c, r, s));

    var tryBlock = currentBlock = make.emptyBlock();
    visit(node.body)(r, s);

    currentBlock = savedBlock;
    breakTargets = savedBreakTargets;
    continueTargets = savedContinueTargets;
    addStatement(make.functionDeclarationStatement(joinName, [], joinBlock));

    if (finallyBlock != null) {
      addStatement(make.functionDeclarationStatement(
          finallyName, [finallyContName], finallyBlock));
    }

    var expr = make.newInstance(make.identifier('Future', 'sync'),
        [make.functionExpression([], tryBlock)]);
    for (var args in catchErrorArgs) {
      expr = make.methodInvocation(expr, 'catchError', args);
    }
    if (finallyBlock != null) {
      var name = newName('e');
      expr = make.methodInvocation(
          expr,
          'then',
          [make.identifier(finallyName),
           make.namedExpression('onError',
               make.functionExpression([name],
                   make.functionInvocation(finallyName,
                       [make.functionExpression([],
                           make.throwExpression(make.identifier(name)))])))]);
    }
    addStatement(make.returnStatement(expr));
  };

  _translateDeclarationList(scanner.Keyword keyword,
      ast.VariableDeclarationList node, s) {
    translateDecl(ast.VariableDeclaration decl, cont) {
      if (decl.initializer == null) {
        return cont(decl);
      } else {
        return visit(decl.initializer)((expr) {
          return cont(make.variableDeclaration(decl.name.name, expr));
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
    addStatement(make.variableDeclarationStatement(keyword, decls));
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

  // [[while (E) S1; S2]] =
  //     var inLoop = false;
  //     break_() {
  //         inLoop = false;
  //         [[S2]]
  //     }
  //     var continue_;
  //     bounce() {
  //         final v = [[E]];
  //         if (v) {
  //             [[S1]];
  //             return continue_();
  //         } else {
  //             inLoop = false;
  //             return break_();
  //         }
  //     continue_ = () {
  //         if (inLoop) return null;
  //         inLoop = true;
  //         var result;
  //         while (inLoop) result = bounce();
  //         return result;
  //     };
  //     return continue_();
  visitWhileStatement(ast.WhileStatement node) => (r, s) {
    var breakName = newName('break');
    var continueName = newName('continue');
    var inLoopName = newName('inLoop');
    var bounceName = newName('bounce');

    var savedBlock = currentBlock;
    var breakBlock = currentBlock = make.emptyBlock();
    s();

    // Though break and continue cannot occur in the loop condition, the
    // targets are added here.  An await in the condition is like a break so
    // the break target should be in scope for the condition.  Continue is
    // set so that the two stacks of targets are handled uniformly.
    var savedBreakTargets = breakTargets;
    var savedContinueTargets = continueTargets;
    breakTargets = _addBreakTarget(labels[node], breakName, inLoopName);
    continueTargets =
        _addContinueTarget(labels[node], continueName, inLoopName);
    var bounceBlock = currentBlock = make.emptyBlock();
    visit(node.condition)((expr) {
      var savedBlock = currentBlock;
      var bodyBlock = currentBlock = make.emptyBlock();

      visit(node.body)((v) {
        addStatement(make.assignmentExpression(
          make.identifier(inLoopName), make.booleanLiteral(false)));
        return r(v);
      }, () {
        addStatement(
            make.returnStatement(make.functionInvocation(continueName)));
      });

      currentBlock = savedBlock;
      addStatement(make.ifStatement(
        expr,
        bodyBlock,
        make.block(
            [make.assignmentExpression(
                 make.identifier(inLoopName), make.booleanLiteral(false)),
             make.returnStatement(make.functionInvocation(breakName))])));
    });

    breakTargets = savedBreakTargets;
    continueTargets = savedContinueTargets;
    currentBlock = savedBlock;
    addStatement(
        make.variableDeclarationStatement(scanner.Keyword.VAR,
            [make.variableDeclaration(inLoopName,
                 make.booleanLiteral(false))]));
    addStatement(make.functionDeclarationStatement(breakName, [], breakBlock));
    addStatement(
        make.variableDeclarationStatement(scanner.Keyword.VAR,
            [make.variableDeclaration(continueName)]));
    addStatement(
        make.functionDeclarationStatement(bounceName, [], bounceBlock));
    _addTrampoline(bounceName, continueName, inLoopName);
    addStatement(make.returnStatement(make.functionInvocation(continueName)));
  };

  visitYieldStatement(ast.YieldStatement node) => unimplemented(node);

  // ---- Expressions ----
  visitAsExpression(ast.AsExpression node) => (s) {
    return visit(node.expression)((expr) {
      return s(make.asExpression(expr, node.type));
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
          return s(make.assignmentExpression(lhs,
              make.binaryExpression(lhsValue,
                  _assignmentToBinaryOperator[node.operator.type],
                  rhs)));
        } else {
          return s(make.assignmentExpression(lhs, rhs, node.operator.type));
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
      return finishAssignment(make.propertyAccess(target, lhs.identifier));
    } else if (lhs is ast.PropertyAccess) {
      if (lhs.target != null) {
        return visit(lhs.target)((target) {
          if (awaits.contains(node.rightHandSide)) {
            target = addTempDeclaration(target);
          }
          return finishAssignment(
              make.propertyAccess(target, lhs.propertyName));
        });
      } else {
        return finishAssignment(make.propertyAccess(null, lhs.propertyName));
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
            return finishAssignment(make.indexExpression(target, index));
         });
        });
      } else {
        return visit(lhs.index)((index) {
          if (awaits.contains(node.rightHandSide)) {
            index = addTempDeclaration(index);
          }
          return finishAssignment(make.indexExpression(null, index));
        });
      }
    } else {
      throw  'Unexpected ${lhs.runtimeType} in assignment: $lhs';
    }
  };

  visitAwaitExpression(ast.AwaitExpression node) => (s) {
    visit(node.expression)((expr) {
      // An await is like a break from all enclosing loops.  The outermost
      // break gives us the list of loop flags that need to be set to false.
      var expression;
      for (var target in breakTargets.reversed) {
        var name = target.inLoopName;
        if (name != null) {
          if (expression == null) expression = make.booleanLiteral(false);
          expression =
              make.assignmentExpression(make.identifier(name), expression);
        }
      }
      if (expression != null) addStatement(expression);
      addStatement(make.returnStatement(
          make.methodInvocation(
                            expr,
                            'then',
                            [reifyExpressionCont(s, 'x')])));
    });
  };

  visitBinaryExpression(ast.BinaryExpression node) => (s) {
    if (node.operator.lexeme == '&&' || node.operator.lexeme == '||') {
      if (awaits.contains(node.rightOperand)) {
        var joinName = newName('join');
        var joinParameterName = newName('x');
        visit(node.leftOperand)((left) {
          var savedBlock = currentBlock;
          var joinBlock = currentBlock = make.emptyBlock();
          s(make.identifier(joinParameterName));

          var rightBlock = currentBlock = make.emptyBlock();
          visit(node.rightOperand)((right) {
            addStatement(make.functionInvocation(joinName,
                [make.conditionalExpression(right,
                                            make.booleanLiteral(true),
                                            make.booleanLiteral(false))]));
          });
          currentBlock = savedBlock;
          addStatement(make.functionDeclarationStatement(
              joinName, [joinParameterName], joinBlock));
          if (node.operator.lexeme == '&&') {
            addStatement(make.ifStatement(
                left,
                rightBlock,
                make.block([make.functionInvocation(joinName,
                                [make.booleanLiteral(false)])])));
          } else {
            addStatement(make.ifStatement(
                left,
                make.block([make.functionInvocation(joinName,
                                [make.booleanLiteral(true)])]),
                rightBlock));
          }
        });
      } else {
        visit(node.leftOperand)((left) {
          visit(node.rightOperand)((right) {
            s(make.binaryExpression(left, node.operator.type, right));
          });
        });
      }
    } else {
      return visit(node.leftOperand)((left) {
        if (awaits.contains(node.rightOperand)) {
          left = addTempDeclaration(left);
        }
       return visit(node.rightOperand)((right) {
          s(make.binaryExpression(left, node.operator.type, right));
        });
      });
    }
  };

  visitCascadeExpression(ast.CascadeExpression node) => (s) {
    visit(node.target)((target) {
      if (node.cascadeSections.any(awaits.contains)) {
        target = addTempDeclaration(target);
      }
      var sections = [];
      var cont = (e) {
        sections.add(make.cascadeSection(e));
        s(make.cascadeExpression(target, sections));
      };
      for (var i = node.cascadeSections.length - 1; i >= 1; --i) {
        var nextCont = cont;
        // The continuation for the i-1 cascade section.
        cont = (e) {
          sections.add(make.cascadeSection(e));
          var nextSection = node.cascadeSections[i];
          if (awaits.contains(nextSection) && sections.isNotEmpty) {
            addStatement(make.cascadeExpression(target, sections));
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
      var savedBlock = currentBlock;
      var joinName = newName('join');
      var joinParameterName = newName('x');
      var joinBlock = currentBlock = make.emptyBlock();
      s(make.identifier(joinParameterName));

      s = (r) {
        addStatement(make.functionInvocation(joinName, [r]));
      };
      var thenBlock = currentBlock = make.emptyBlock();
      visit(node.thenExpression)(s);
      var elseBlock = currentBlock = make.emptyBlock();
      visit(node.elseExpression)(s);
      currentBlock = savedBlock;
      addStatement(make.functionDeclarationStatement(
          joinName, [joinParameterName], joinBlock));
      addStatement(make.ifStatement(expr, thenBlock, elseBlock));
    });
  };

  visitFunctionExpression(ast.FunctionExpression node) => (s) {
    node.body = new AsyncTransformer().visit(node.body);
    return s(make.parenthesizedExpression(node));
  };

  visitFunctionExpressionInvocation(
      ast.FunctionExpressionInvocation node) => (s) {
    return visit(node.function)((rator) {
      if (awaits.contains(node.argumentList)) {
        rator = addTempDeclaration(rator);
      }
      return _translateExpressionList(node.argumentList.arguments, (rands) {
        return s(make.functionInvocation(rator, rands));
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
        s(make.indexExpression(e0, e1));
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
      s(make.newInstance(node.constructorName,
                         rands,
                         scanner.Keyword.keywords[node.keyword.lexeme]));
    });
  };

  visitIsExpression(ast.IsExpression node) => (s) {
    return visit(node.expression)((expr) {
      return s(make.isExpression(expr, node.notOperator != null, node.type));
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
      s(make.adjacentStrings(strings));
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
          elements.add(make.interpolationExpression(exprs[index++]));
        } else {
          elements.add(element);
        }
      }
      // The translation of adjacent strings relies on never passing a
      // non-StringLiteral to the success continuation here.
      s(make.stringInterpolation(elements));
    });
  };

  visitSymbolLiteral(ast.SymbolLiteral node) => (s) {
    return s(node);
  };

  // ---- TypedLiterals ----
  visitListLiteral(ast.ListLiteral node) => (s) {
    _translateExpressionList(node.elements, (elts) {
      s(make.listLiteral(elts));
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
      s(make.mapLiteral(entries));
    });
  };

  visitMethodInvocation(ast.MethodInvocation node) => (s) {
    if (node.target != null) {
      visit(node.target)((rator) {
        if (awaits.contains(node.argumentList)) {
          rator = addTempDeclaration(rator);
        }
        _translateExpressionList(node.argumentList.arguments, (rands) {
          s(make.methodInvocation(rator, node.methodName.name, rands));
        });
      });
    } else {
      _translateExpressionList(node.argumentList.arguments, (rands) {
        s(make.methodInvocation(null, node.methodName.name, rands));
      });
    }
  };

  visitNamedExpression(ast.NamedExpression node) => (s) {
    return visit(node.expression)((expr) {
      return s(make.namedExpression(node.name, expr));
    });
  };

  visitParenthesizedExpression(ast.ParenthesizedExpression node) => (s) {
    return visit(node.expression)(s);
  };

  visitPostfixExpression(ast.PostfixExpression node) => (s) {
    return visit(node.operand)((expr) {
      return s(make.postfixExpression(expr, node.operator.type));
    });
  };

  visitPrefixExpression(ast.PrefixExpression node) => (s) {
    return visit(node.operand)((expr) {
      return s(make.prefixExpression(node.operator.type, expr));
    });
  };

  visitPropertyAccess(ast.PropertyAccess node) => (s) {
    return visit(node.target)((expr) {
      return s(make.propertyAccess(expr, node.propertyName));
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
      return s(make.throwExpression(expr));
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
