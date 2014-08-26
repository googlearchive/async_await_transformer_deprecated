library async_await.src.xform;

import 'package:analyzer/src/generated/ast.dart' as ast;
import 'package:analyzer/src/generated/scanner.dart' as scanner;
import 'package:analyzer/src/generated/testing/ast_factory.dart';
import 'package:analyzer/src/generated/testing/token_factory.dart';

class AnalysisVisitor extends ast.GeneralizingAstVisitor<bool> {
  Set<ast.AstNode> awaits = new Set<ast.AstNode>();

  bool maybeAdd(ast.AstNode node, bool shouldAdd) {
    if (shouldAdd) awaits.add(node);
    return shouldAdd;
  }

  bool visit(ast.AstNode node) => node.accept(this);

  bool visitNode(ast.AstNode node) {
    throw 'Unimplemented(${node.runtimeType})';
  }

  bool visitCompilationUnit(ast.CompilationUnit node) {
    node.declarations.forEach(visit);
    return false;
  }

  bool visitClassDeclaration(ast.ClassDeclaration node) {
    node.members.forEach(visit);
    return false;
  }

  bool visitConstructorDeclaration(ast.ConstructorDeclaration node) {
    return false;
  }

  bool visitFieldDeclaration(ast.FieldDeclaration node) {
    return false;
  }

  bool visitMethodDeclaration(ast.MethodDeclaration node) {
    visit(node.body);
    return false;
  }

  bool visitFunctionDeclaration(ast.FunctionDeclaration node) {
    visit(node.functionExpression.body);
    return false;
  }

  bool visitTopLevelVariableDeclaration(ast.TopLevelVariableDeclaration node) {
    return false;
  }

  bool visitBlockFunctionBody(ast.BlockFunctionBody node) {
    if (node.isSynchronous || node.isGenerator) return false;
    visit(node.block);
    return false;
  }

  bool visitExpressionFunctionBody(ast.ExpressionFunctionBody node) {
    if (node.isSynchronous || node.isGenerator) return false;
    visit(node.expression);
    return false;
  }

  bool visitEmptyFunctionBody(ast.EmptyFunctionBody node) {
    return false;
  }

  bool visitFunctionTypeAlias(ast.FunctionTypeAlias node) {
    return false;
  }

  bool visitArgumentList(ast.ArgumentList node) {
    var result = false;
    node.arguments.forEach((e) {
      result = visit(e) || result;
    });
    return maybeAdd(node, result);
  }

  bool visitCatchClause(ast.CatchClause node) {
    assert(node.onKeyword == null);
    assert(node.stackTraceParameter == null);
    return maybeAdd(node, visit(node.body));
  }

  bool visitVariableDeclarationList(ast.VariableDeclarationList node) {
    var result = false;
    node.variables.forEach((d) {
      result = visit(d) || result;
    });
    return maybeAdd(node, result);
  }

  bool visitVariableDeclaration(ast.VariableDeclaration node) {
    if (node.initializer == null) return false;
    return maybeAdd(node, visit(node.initializer));
  }

  // Statements
  bool visitBlock(ast.Block node) {
    var result = false;
    node.statements.forEach((s) {
      result = visit(s) || result;
    });
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

  bool visitExpressionStatement(ast.ExpressionStatement node) {
    return maybeAdd(node, visit(node.expression));
  }

  bool visitForStatement(ast.ForStatement node) {
    var result = false;
    if (node.variables != null) {
      result = visit(node.variables);
    } else if (node.initialization != null) {
      result = visit(node.initialization);
    }
    if (node.condition != null) {
      result = visit(node.condition) || result;
    }
    result = visit(node.body) || result;
    if (node.updaters != null) {
      node.updaters.forEach((e) {
        result = visit(e) || result;
      });
    }
    return maybeAdd(node, result);
  }

  bool visitForEachStatement(ast.ForEachStatement node) {
    var result = visit(node.iterator);
    return maybeAdd(node, visit(node.body) || result);
  }

  bool visitIfStatement(ast.IfStatement node) {
    var result = visit(node.condition);
    result = visit(node.thenStatement) || result;
    if (node.elseStatement != null) {
      result = visit(node.elseStatement) || result;
    }
    return maybeAdd(node, result);
  }

  bool visitReturnStatement(ast.ReturnStatement node) {
    if (node.expression == null) return false;
    return maybeAdd(node, visit(node.expression));
  }

  bool visitTryStatement(ast.TryStatement node) {
    var result = visit(node.body);
    if (node.catchClauses.isNotEmpty) {
      assert(node.catchClauses.length == 1);
      result = visit(node.catchClauses.first) || result;
    }
    if (node.finallyBlock != null) {
      result = visit(node.finallyBlock) || result;
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

  // Expressions
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

  bool visitDoubleLiteral(ast.DoubleLiteral node) {
    return false;
  }

  bool visitFunctionExpression(ast.FunctionExpression node) {
    visit(node.body);
    return false;
  }

  bool visitIndexExpression(ast.IndexExpression node) {
    var result = visit(node.target);
    return maybeAdd(node, visit(node.index) || result);
  }

  bool visitInstanceCreationExpression(ast.InstanceCreationExpression node) {
    return maybeAdd(node, visit(node.argumentList));
  }

  bool visitIntegerLiteral(ast.IntegerLiteral node) {
    return false;
  }

  bool visitListLiteral(ast.ListLiteral node) {
    var result = false;
    node.elements.forEach((e) {
      result = visit(e) || result;
    });
    return maybeAdd(node, result);
  }

  bool visitMapLiteral(ast.MapLiteral node) {
    var result = false;
    node.entries.forEach((entry) {
      result = visit(entry) || result;
    });
    return maybeAdd(node, result);
  }

  bool visitMapLiteralEntry(ast.MapLiteralEntry node) {
    var result = visit(node.key);
    return maybeAdd(node, visit(node.value) || result);
  }

  bool visitMethodInvocation(ast.MethodInvocation node) {
    var result = node.target != null && visit(node.target);
    result = visit(node.argumentList) || result;
    return maybeAdd(node, result);
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

  bool visitPrefixedIdentifier(ast.PrefixedIdentifier node) {
    return false;
  }

  bool visitPropertyAccess(ast.PropertyAccess node) {
    return maybeAdd(node, visit(node.target));
  }

  bool visitSimpleStringLiteral(ast.SimpleStringLiteral node) {
    return false;
  }

  bool visitSimpleIdentifier(ast.SimpleIdentifier node) {
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
    node.elements.forEach((elt) {
      result = visit(elt) || result;
    });
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
          scanner.Keyword.VAR,
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
  final Set<ast.AstNode> awaits;

  ast.Block currentBlock;
  List<ast.Expression> breakTargets;
  List<ast.Expression> continueTargets;

  AsyncTransformer(this.awaits);

  visit(ast.AstNode node) => node.accept(this);

  int counter = 0;

  // TODO: Safely generate fresh identifiers.
  String newName(String base) => '$base${counter++}';

  /// Insert a declaration with initial value [expr] in [currentBlock] and
  /// return the fresh name.
  ///
  /// No declaration is added if the expression is already a value and [force]
  /// is false.
  ast.Expression addDeclaration(String base, ast.Expression expr,
      {force: false}) {
    if (!force && expr is ast.Literal) {
      return expr;
    }
    var name = newName(base);
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

  ast.FunctionExpression reifyExpressionCont(s, f) {
    var savedBlock = currentBlock;
    var exnBlock = currentBlock = emptyBlock();
    var exnName = newName('e');
    f(identifier(exnName));

    currentBlock = emptyBlock();
    var name = newName('x');
    s(identifier(name));

    var fun =
        functionExpression(
            [name],
            AstFactory.tryStatement2(
                currentBlock,
                [AstFactory.catchClause(exnName, exnBlock.statements)]));
    currentBlock = savedBlock;
    return fun;
  }

  ast.FunctionExpression reifyErrorCont(f) {
    var savedBlock = currentBlock;
    currentBlock = emptyBlock();
    String name = newName('e');
    f(identifier(name));
    var fun = functionExpression([name], currentBlock);
    currentBlock = savedBlock;
    return fun;
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
    if (node.isSynchronous || node.isGenerator) return node;

    counter = 0;
    currentBlock = emptyBlock();
    breakTargets = <ast.Expression>[];
    continueTargets = <ast.Expression>[];
    var result = newName('completer');
    visit(node.block)((e) {
      addStatement(methodInvocation(result, 'completeError', [e]));
    }, (v) {
      addStatement(methodInvocation(result, 'complete', [v]));
    }, () {
      addStatement(methodInvocation(result, 'complete', [nullLiteral()]));
    });

    String exnName = newName('e');
    return AstFactory.blockFunctionBody2(
        [variableDeclaration(
              result,
              AstFactory.instanceCreationExpression2(
                  scanner.Keyword.NEW,
                  AstFactory.typeName4('Completer', []),
                  [])),
          AstFactory.expressionStatement(
              AstFactory.methodInvocation2(
                  'scheduleMicrotask',
                  [functionExpression(
                        [],
                        AstFactory.tryStatement2(
                            currentBlock,
                            [AstFactory.catchClause(
                                  exnName,
                                  [AstFactory.expressionStatement(
                                        AstFactory.methodInvocation(
                                            identifier(result),
                                            'completeError',
                                            [identifier(exnName)]))])]))])),
          AstFactory.returnStatement2(
              AstFactory.propertyAccess2(identifier(result), 'future'))]);
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
  visitAssertStatement(ast.AssertStatement node) => unimplemented(node);

  visitBlock(ast.Block node) => (f, r, s) {
    for (var stmt in node.statements.reversed) {
      var nextCont = s;
      s = () => visit(stmt)(f, r, nextCont);
    }
    return s();
  };

  visitBreakStatement(ast.BreakStatement node) => (f, r, s) {
    if (node.label != null) unimplemented(node);
    addStatement(AstFactory.functionExpressionInvocation(
        breakTargets.last, [nullLiteral()]));
  };

  visitContinueStatement(ast.ContinueStatement node) => (f, r, s) {
    if (node.label != null) unimplemented(node);
    addStatement(AstFactory.functionExpressionInvocation(
            continueTargets.last, [nullLiteral()]));
  };

  visitDoStatement(ast.DoStatement node) => (f, r, s) {
    var breakName = newName('break');
    var continueName = newName('continue');
    var loopName = newName('loop');

    var savedBlock = currentBlock;
    var breakBlock = currentBlock = emptyBlock();
    s();

    var continueBlock = currentBlock = emptyBlock();
    visit(node.condition)(f, (expr) {
      addStatement(AstFactory.ifStatement2(
          expr,
          block([AstFactory.methodInvocation(
              AstFactory.functionExpressionInvocation(
                  AstFactory.identifier(identifier('Future'),
                                        identifier('wait')),
                  [AstFactory.listLiteral([])]),
              'then',
              [identifier(loopName)])]),
          block([AstFactory.functionExpressionInvocation(
              identifier(breakName), [nullLiteral()])])));
    });

    var loopBlock = currentBlock = emptyBlock();
    addStatement(
        AstFactory.functionDeclarationStatement(null, null, continueName,
            functionExpression(['_'], continueBlock)));
    breakTargets.add(identifier(breakName));
    continueTargets.add(identifier(continueName));
    visit(node.body)(f, r, () {
      addStatement(
          AstFactory.functionExpressionInvocation(
              identifier(continueName), [nullLiteral()]));
    });
    breakTargets.removeLast();
    continueTargets.removeLast();

    currentBlock = savedBlock;
    addStatement(
        AstFactory.functionDeclarationStatement(null, null, breakName,
            functionExpression(['_'], breakBlock)));
    addStatement(
        AstFactory.functionDeclarationStatement(null, null, loopName,
            functionExpression(['_'], loopBlock)));
    addStatement(
        AstFactory.functionExpressionInvocation(
            identifier(loopName), [nullLiteral()]));
  };

  visitEmptyStatement(ast.EmptyStatement node) => (f, r, s) {
    return s();
  };

  visitExpressionStatement(ast.ExpressionStatement node) => (f, r, s) {
    return visit(node.expression)(f, (expr) {
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

  _translateForUpdaters(List<ast.Expression> exprs, f, s) {
    var cont = s;
    for (var expr in exprs.reversed) {
      var nextCont = cont;
      cont = () {
        visit(expr)(f, (expr) {
          addStatement(expr);
          return nextCont();
        });
      };
     }
     return cont();
   }

  _translateForDeclarations(List<ast.VariableDeclaration> decls, f, s) {
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
        if (copiedSeenAwait) e = addDeclaration('v', e);
        exprs.add(e);
        return (nextExpr == null)
            ? nextCont(nullLiteral())
            : visit(nextExpr)(f, nextCont);
      };
    }
    var expr = decls.first.initializer;
    return (expr == null)
        ? cont(nullLiteral())
        :  visit(expr)(f, cont);
  }

 visitForStatement(ast.ForStatement node) => (f, r, s) {
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
      addStatement(
          AstFactory.methodInvocation(
              AstFactory.functionExpressionInvocation(
                  AstFactory.identifier(identifier('Future'),
                                        identifier('wait')),
                  [AstFactory.listLiteral([])]),
              'then',
              [functionExpression (['_'],
                  AstFactory.functionExpressionInvocation(
                      identifier(loopName),
                      parameters))]));
    }
    if (node.updaters != null) {
      _translateForUpdaters(node.updaters, f, trampoline);
    } else {
      trampoline();
    }

    var bodyBlock = currentBlock = emptyBlock();
    breakTargets.add(identifier(breakName));
    continueTargets.add(identifier(continueName));
    visit(node.body)(f, r, () {
      addStatement(
          AstFactory.functionExpressionInvocation(
              identifier(continueName), [nullLiteral()]));
    });
    breakTargets.removeLast();
    continueTargets.removeLast();

    var loopBlock = currentBlock = emptyBlock();
    addStatement(
        AstFactory.functionDeclarationStatement(null, null, continueName,
            functionExpression(['_'], continueBlock)));
    if (node.condition != null) {
      visit(node.condition)(f, (expr) {
        addStatement(AstFactory.ifStatement2(
          expr,
          bodyBlock,
          block([AstFactory.functionExpressionInvocation(
              identifier(breakName), [nullLiteral()])])));
      });
    } else {
      addStatement(bodyBlock);
    }

    currentBlock = savedBlock;
    addStatement(
        AstFactory.functionDeclarationStatement(null, null, breakName,
            functionExpression(['_'], breakBlock)));
    addStatement(
        AstFactory.functionDeclarationStatement(null, null, loopName,
            functionExpression(parameters.map((e) => e.name).toList(),
                loopBlock)));
    if (node.variables != null) {
      assert(node.variables.variables.isNotEmpty);
      return _translateForDeclarations(node.variables.variables, f, (args) {
        assert(args.length == parameters.length);
        addStatement(AstFactory.functionExpressionInvocation(
            identifier(loopName), args));
      });
    } else if (node.initialization != null) {
      assert(parameters.isEmpty);
      return visit(node.initialization)(f, (expr) {
        addStatement(expr);
        addStatement(AstFactory.functionExpressionInvocation(
            identifier(loopName), []));
      });
    } else {
      assert(parameters.isEmpty);
      return addStatement(AstFactory.functionExpressionInvocation(
          identifier(loopName), []));
    }
  };

  visitFunctionDeclarationStatement(ast.FunctionDeclarationStatement node) {
    return unimplemented(node);
  }

  visitIfStatement(ast.IfStatement node) => (f, r, s) {
    var hasElse = node.elseStatement != null;
    return visit(node.condition)(f, (expr) {
      var joinName = newName('join');
      var joinFun = reifyStatementCont(s);
      addStatement(
          AstFactory.functionDeclarationStatement(
              null, null, joinName, joinFun));
      s = () {
        addStatement(AstFactory.methodInvocation2(joinName, []));
      };
      var savedBlock = currentBlock;
      var thenBlock = currentBlock = emptyBlock();
      visit(node.thenStatement)(f, r, s);
      var elseBlock = currentBlock = emptyBlock();
      if (hasElse) {
        visit(node.elseStatement)(f, r, s);
      } else {
        s();
      }
      currentBlock = savedBlock;
      addStatement(AstFactory.ifStatement2(expr, thenBlock, elseBlock));
    });
  };

  visitLabeledStatement(ast.LabeledStatement node) => unimplemented(node);

  visitRethrowExpression(ast.RethrowExpression node) => unimplemented(node);

  visitReturnStatement(ast.ReturnStatement node) => (f, r, s) {
    return (node.expression == null)
        ? r(AstFactory.nullLiteral())
        : visit(node.expression)(f, (v) => r(v));
  };

  visitSwitchStatement(ast.SwitchStatement node) => unimplemented(node);

  visitCatchClause(ast.CatchClause node) => (f, r, s) {
    // TODO(kmillikin): handle 'on T catch' clauses.
    return visit(node.body)(f, r, s);
  };

  visitTryStatement(ast.TryStatement node) => (f, r, s) {
    var joinName = newName('join');
    ast.FunctionExpression joinFun = reifyStatementCont(s);
    joinFun.parameters = AstFactory.formalParameterList(
        [AstFactory.simpleFormalParameter3('_')]);
    addStatement(
        AstFactory.functionDeclarationStatement(null, null, joinName, joinFun));

    var finallyName = newName('finally');
    var finallyContName = newName('cont');
    var finallyValueName = newName('v');
    var savedBlock = currentBlock;
    var finallyBlock = currentBlock = emptyBlock();
    s = () {
      addStatement(AstFactory.functionExpressionInvocation(
              identifier(finallyContName),
              [identifier(finallyValueName)]));
    };
    if (node.finallyBlock != null) {
      visit(node.finallyBlock)(f, r, s);
    } else {
      s();
    }

    var catchName = newName('catch');
    var exnName = node.catchClauses.isEmpty ?
        newName('e') :
        node.catchClauses.first.exceptionParameter.name;
    var catchBlock = currentBlock = emptyBlock();
    var savedBreakTargets = breakTargets;
    var savedContinueTargets = continueTargets;
    breakTargets = breakTargets.map((expr) {
      return AstFactory.parenthesizedExpression(
              functionExpression(['_'],
                  AstFactory.functionExpressionInvocation(
                      identifier(finallyName), [expr, identifier('_')])));
    }).toList();
    continueTargets = continueTargets.map((expr) {
      return AstFactory.parenthesizedExpression(
          functionExpression(['_'],
               AstFactory.functionExpressionInvocation(
                  identifier(finallyName), [expr, identifier('_')])));
    }).toList();
    if (node.catchClauses.isNotEmpty) {
      assert(node.catchClauses.length == 1);
      visit(node.catchClauses.first)((e) {
        addStatement(
            AstFactory.methodInvocation2(finallyName, [reifyErrorCont(f), e]));
      }, (v) {
        addStatement(
            AstFactory.methodInvocation2(finallyName, [reifyErrorCont(r), v]));
      }, () {
        addStatement(
            AstFactory.methodInvocation2(
                finallyName,
                [identifier(joinName), AstFactory.nullLiteral()]));
      });
    } else {
      addStatement(
          AstFactory.methodInvocation2(
              finallyName,
              [identifier(joinName), AstFactory.nullLiteral()]));
    }

    var tryBlock = currentBlock = emptyBlock();
    visit(node.body)((e) {
      addStatement(AstFactory.methodInvocation2(catchName, [e]));
    }, (v) {
      addStatement(
          AstFactory.methodInvocation2(finallyName, [reifyErrorCont(r), v]));
    }, () {
      addStatement(
          AstFactory.expressionStatement(
              AstFactory.methodInvocation2(
                  finallyName,
                  [identifier(joinName), AstFactory.nullLiteral()])));
    });

    currentBlock = savedBlock;
    breakTargets = savedBreakTargets;
    continueTargets = savedContinueTargets;
    addStatement(
        AstFactory.functionDeclarationStatement(
            null,
            null,
            finallyName,
            functionExpression([finallyContName, finallyValueName], finallyBlock)));

    addStatement(
        AstFactory.functionDeclarationStatement(
            null,
            null,
            catchName,
            functionExpression([exnName], catchBlock)));
    var name = newName('e');
    addStatement(
        AstFactory.tryStatement2(
            tryBlock,
            [AstFactory.catchClause(
                  name,
                  [AstFactory.expressionStatement(
                        AstFactory.methodInvocation2(
                            catchName, [identifier(name)]))])]));
  };

  _translateDeclarationList(scanner.Keyword keyword,
      ast.VariableDeclarationList node) => (f, s) {
    translateDecl(ast.VariableDeclaration decl, cont) {
      if (decl.initializer == null) {
        return cont(decl);
      } else {
        return visit(decl.initializer)(f, (expr) {
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
  };

  void _residualizeDeclarationList(scanner.Keyword keyword,
      List<ast.VariableDeclaration> decls) {
    if (decls.isEmpty) return;
    addStatement(AstFactory.variableDeclarationStatement2(keyword, decls));
  }

  visitVariableDeclarationStatement(
      ast.VariableDeclarationStatement node) => (f, r, s) {
    var keyword = scanner.Keyword.keywords[node.variables.keyword.lexeme];
    return _translateDeclarationList(keyword, node.variables)(f, (decls) {
      _residualizeDeclarationList(keyword, decls);
      return s();
    });
  };

  visitWhileStatement(ast.WhileStatement node) => (f, r, s) {
    var breakName = newName('break');
    var continueName = newName('continue');

    var savedBlock = currentBlock;
    var breakBlock = currentBlock = emptyBlock();
    s();

    var continueBlock = currentBlock = emptyBlock();
    visit(node.condition)(f, (expr) {
      var savedBlock = currentBlock;
      var bodyBlock = currentBlock = emptyBlock();
      breakTargets.add(identifier(breakName));
      continueTargets.add(identifier(continueName));
      visit(node.body)(f, r, () {
        addStatement(
            AstFactory.functionExpressionInvocation(
              identifier(continueName), [nullLiteral()]));
      });
      breakTargets.removeLast();
      continueTargets.removeLast();

      currentBlock = savedBlock;
      addStatement(AstFactory.ifStatement2(
        expr,
        // Trampoline the body via Future.wait.
        block([AstFactory.methodInvocation(
            AstFactory.functionExpressionInvocation(
                AstFactory.identifier(identifier('Future'),
                                      identifier('wait')),
                [AstFactory.listLiteral([])]),
            'then',
            [functionExpression(['_'], bodyBlock)])]),
        block([AstFactory.functionExpressionInvocation(
            identifier(breakName), [nullLiteral()])])));
    });

    currentBlock = savedBlock;
    addStatement(
        AstFactory.functionDeclarationStatement(null, null, breakName,
            functionExpression(['_'], breakBlock)));
    addStatement(
        AstFactory.functionDeclarationStatement(null, null, continueName,
            functionExpression(['_'], continueBlock)));
    addStatement(
        AstFactory.functionExpressionInvocation(
            identifier(continueName), [nullLiteral()]));
  };

  visitYieldStatement(ast.YieldStatement node) => unimplemented(node);

  // ---- Expressions ----
  visitAsExpression(ast.AsExpression node) => unimplemented(node);

  visitAssignmentExpression(ast.AssignmentExpression node) => (f, s) {
    assert(node.leftHandSide is ast.SimpleIdentifier);
    // TODO(kmillikin): handle compound assignments.
    visit(node.rightHandSide)(f, (expr) {
      return s(AstFactory.assignmentExpression(
              node.leftHandSide, node.operator.type, expr));
    });
  };

  visitAwaitExpression(ast.AwaitExpression node) => (f, s) {
    if (awaits.contains(node.expression)) {
      visit(node.expression)(f, (expr) {
        addStatement(
            AstFactory.methodInvocation(
                expr,
                'then',
                [reifyExpressionCont(s, f),
                  AstFactory.namedExpression2('onError', reifyErrorCont(f))]));
      });
    } else {
      addStatement(
          AstFactory.methodInvocation(
              node.expression,
              'then',
              [reifyExpressionCont(s, f),
                AstFactory.namedExpression2('onError', reifyErrorCont(f))]));
    }
  };

  visitBinaryExpression(ast.BinaryExpression node) => (f, s) {
    // TODO: Is this even possible?
    assert(node.operator.lexeme != '||' && node.operator.lexeme != '&&');
    return visit(node.leftOperand)(f, (left) {
      if (awaits.contains(node.rightOperand)) {
        left = addDeclaration('v', left);
      }
      return visit(node.rightOperand)(f, (right) {
        s(AstFactory.binaryExpression(left, node.operator.type, right));
      });
    });
  };

  visitCascadeExpression(ast.CascadeExpression node) => unimplemented(node);

  visitConditionalExpression(ast.ConditionalExpression node) {
    unimplemented(node);
  }

  visitFunctionExpression(ast.FunctionExpression node) => (f, s) {
    // TODO(kmillikin): Handle async bodies.
    return s(node);
  };

  visitFunctionExpressionInvocation(ast.FunctionExpressionInvocation node) {
    unimplemented(node);
  }

  // ---- Identifiers ----
  visitSimpleIdentifier(ast.SimpleIdentifier node) => (f, s) {
    return s(node);
  };

  visitPrefixedIdentifier(ast.PrefixedIdentifier node) => (f, s) {
    return s(node);
  };

  visitIndexExpression(ast.IndexExpression node) => (f, s) {
    visit(node.target)(f, (e0) {
      if (awaits.contains(node.index)) {
        e0 = addDeclaration('v', e0);
      }
      visit(node.index)(f, (e1) {
        s(AstFactory.indexExpression(e0, e1));
      });
    });
  };

  _translateExpressionList(ast.NodeList<ast.Expression> exprs) => (f, s) {
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
          var value = addDeclaration('v', v);
          args.add(value);
          visit(expr)(f, current);
        };
      } else {
        cont = (v) {
          args.add(v);
          visit(expr)(f, current);
        };
      }
    }
    return visit(exprs.first)(f, cont);
  };

  visitInstanceCreationExpression(
      ast.InstanceCreationExpression node) => (f, s) {
    _translateExpressionList(node.argumentList.arguments)(f, (rands) {
      s(AstFactory.instanceCreationExpression(
              scanner.Keyword.keywords[node.keyword.lexeme],
              node.constructorName,
              rands));
    });
  };

  visitIsExpression(ast.IsExpression node) => unimplemented(node);

  // ---- Literals ----
  visitBooleanLiteral(ast.BooleanLiteral node) => (f, s) {
    s(node);
  };

  visitDoubleLiteral(ast.DoubleLiteral node) => (f, s) {
    s(node);
  };

  visitIntegerLiteral(ast.IntegerLiteral node) => (f, s) {
    s(node);
  };

  visitNullLiteral(ast.NullLiteral node) => (f, s) {
    return s(node);
  };

  // ---- StringLiterals ----
  visitAdjacentStrings(ast.AdjacentStrings node) => unimplemented(node);

  visitSimpleStringLiteral(ast.SimpleStringLiteral node) => (f, s) {
    return s(node);
  };

  visitStringInterpolation(ast.StringInterpolation node) => (f, s) {
    var list = new ast.NodeList<ast.Expression>(node);
    for (var element in node.elements) {
      if (element is ast.InterpolationExpression) {
        list.add(element.expression);
      } else {
        assert(element is ast.InterpolationString);
      }
    }
    _translateExpressionList(list)(f, (exprs) {
      var elements = <ast.InterpolationElement>[];
      int index = 0;
      for (var element in node.elements) {
        if (element is ast.InterpolationExpression) {
          elements.add(AstFactory.interpolationExpression(exprs[index++]));
        } else {
          elements.add(element);
        }
      }
      s(AstFactory.string(elements));
    });
  };

  visitSymbolLiteral(ast.SymbolLiteral node) => (f, s) {
    return s(node);
  };

  // ---- TypedLiterals ----
  visitListLiteral(ast.ListLiteral node) => (f, s) {
    _translateExpressionList(node.elements)(f, (elts) {
      s(AstFactory.listLiteral(elts));
    });
  };

  visitMapLiteral(ast.MapLiteral node) => (f, s) {
    var list = new ast.NodeList<ast.Expression>(node);
    for (var entry in node.entries) {
      list.add(entry.key);
      list.add(entry.value);
    }
    _translateExpressionList(list)(f, (exprs) {
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

  visitMethodInvocation(ast.MethodInvocation node) => (f, s) {
    if (node.target != null) {
      visit(node.target)(f, (rator) {
        if (awaits.contains(node.argumentList)) {
          rator = addDeclaration('v', rator);
        }
        _translateExpressionList(node.argumentList.arguments)(f, (rands) {
          s(AstFactory.methodInvocation(rator, node.methodName.name, rands));
        });
      });
    } else {
      _translateExpressionList(node.argumentList.arguments)(f, (rands) {
        s(AstFactory.methodInvocation2(node.methodName.name, rands));
      });
    }
  };

  visitNamedExpression(ast.NamedExpression node) => (f, s) {
    return visit(node.expression)(f, (expr) {
      return s(AstFactory.namedExpression(node.name, expr));
    });
  };

  visitParenthesizedExpression(ast.ParenthesizedExpression node) => (f, s) {
    return visit(node.expression)(f, s);
  };

  visitPostfixExpression(ast.PostfixExpression node) => unimplemented(node);

  visitPrefixExpression(ast.PrefixExpression node) => unimplemented(node);

  visitPropertyAccess(ast.PropertyAccess node) => (f, s) {
    return visit(node.target)(f, (expr) {
      return s(AstFactory.propertyAccess(expr, node.propertyName));
    });
  };

  visitSuperExpression(ast.SuperExpression node) => unimplemented(node);

  visitThisExpression(ast.ThisExpression node) => unimplemented(node);

  visitThrowExpression(ast.ThrowExpression node) => (f, s) {
    return visit(node.expression)(f, (e) => f(e));
  };

  unreachable(node) => throw 'Unreachable(${node.runtimeType})';
  visitAnnotation(node) => unreachable(node);
  visitArgumentList(node) => unreachable(node);
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