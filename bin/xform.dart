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
    throw 'TODO(${node.runtimeType})';
  }

  bool visitCompilationUnit(ast.CompilationUnit node) {
    node.declarations.forEach(visit);
    return false;
  }

  bool visitFunctionDeclaration(ast.FunctionDeclaration node) {
    return visit(node.functionExpression.body);
  }

  bool visitBlockFunctionBody(ast.BlockFunctionBody node) {
    if (node.keyword == null) return false;
    if (node.star != null) {
      throw 'TODO(${node.keyword}${node.star})';
    }
    return visit(node.block);
  }

  bool visitExpressionFunctionBody(ast.ExpressionFunctionBody node) {
    if (node.keyword == null) return false;
    return visit(node.expression);
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

  bool visitExpressionStatement(ast.ExpressionStatement node) {
    return maybeAdd(node, visit(node.expression));
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

  bool visitFunctionExpression(ast.FunctionExpression node) {
    visit(node.body);
    return false;
  }

  bool visitInstanceCreationExpression(ast.InstanceCreationExpression node) {
    return maybeAdd(node, visit(node.argumentList));
  }

  bool visitIntegerLiteral(ast.IntegerLiteral node) {
    return false;
  }

  bool visitMethodInvocation(ast.MethodInvocation node) {
    var result = node.target != null && visit(node.target);
    result = visit(node.argumentList) || result;
    return maybeAdd(node, result);
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

  bool visitSimpleStringLiteral(ast.SimpleStringLiteral node) {
    return false;
  }

  bool visitSimpleIdentifier(ast.SimpleIdentifier node) {
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

typedef void StatementCont();
typedef void ReturnCont(ast.Expression expr);
typedef void DeclarationListCont(List<ast.VariableDeclaration> decls);

typedef void StatementTransformer(ErrorCont f, ReturnCont r, StatementCont s);
typedef void DeclarationListTransformer(ErrorCont f, DeclarationListCont s);

class AsyncTransformer extends ast.RecursiveAstVisitor<StatementTransformer> {
  final Set<ast.AstNode> awaits;
  AsyncExpressionTransformer expressionTransformer;

  ast.Block currentBlock;

  AsyncTransformer(this.awaits) {
    expressionTransformer = new AsyncExpressionTransformer(this);
  }

  visit(ast.AstNode node) => node.accept(this);
  ExpressionTransformer visitExpression(ast.Expression expr) {
    return expressionTransformer.visit(expr);
  }

  int counter = 0;

  // TODO: Safely generate fresh identifiers.
  String newName(String base) => '$base${counter++}';

  List<ast.Expression> breakTargets;
  List<ast.Expression> continueTargets;

  /// Insert a declaration with initial value [expr] in [currentBlock] and
  /// return the fresh name.
  ///
  /// No declaration is added if the expression is already a value and [force]
  /// is false.
  ast.Expression addDeclaration(String base, ast.Expression expr, {force:
      false}) {
    if (!force &&
        ((expr is ast.SimpleIdentifier || expr is ast.SimpleStringLiteral))) {
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

  visitNode(ast.AstNode node) {
    throw 'AsyncTransformer(${node.runtimeType})';
  }

  ast.FunctionExpression reifyErrorCont(ErrorCont f) {
    var savedBlock = currentBlock;
    currentBlock = emptyBlock();
    String name = newName('e');
    f(identifier(name));
    var fun = functionExpression([name], currentBlock);
    currentBlock = savedBlock;
    return fun;
  }

  ast.FunctionExpression reifyStatementCont(StatementCont s) {
    var savedBlock = currentBlock;
    currentBlock = emptyBlock();
    s();
    var fun = functionExpression([], currentBlock);
    currentBlock = savedBlock;
    return fun;
  }

  visitCompilationUnit(ast.CompilationUnit node) {
    return new ast.CompilationUnit(
        node.beginToken,
        node.scriptTag,
        node.directives,
        node.declarations.map(visit).toList(growable: false),
        node.endToken);
  }

  visitFunctionDeclaration(ast.FunctionDeclaration node) {
    return new ast.FunctionDeclaration(
        node.documentationComment,
        node.metadata,
        node.externalKeyword,
        node.returnType,
        node.propertyKeyword,
        node.name,
        new ast.FunctionExpression(
            node.functionExpression.parameters,
            visit(node.functionExpression.body)));
  }

  visitBlockFunctionBody(ast.BlockFunctionBody node) {
    if (node.keyword == null) return node;
    if (node.star != null) {
      throw 'TODO(${node.keyword}${node.star})';
    }
    counter = 0;
    currentBlock = emptyBlock();
    breakTargets = <ast.Expression>[];
    continueTargets = <ast.Expression>[];
    var result = newName('result');
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

  visitExpressionFunctionBody(ast.ExpressionFunctionBody node) {
    if (node.keyword == null) return node;
    return visit(AstFactory.blockFunctionBody2(
        [AstFactory.returnStatement2(node.expression)])
        ..keyword = node.keyword);
  }

  visitCatchClause(ast.CatchClause node) => (f, r, s) {
    return visit(node.body)(f, r, s);
  };

  visitVariableDeclarationList(ast.VariableDeclarationList node) {
    return new ast.VariableDeclarationList(
        node.documentationComment,
        node.metadata,
        node.keyword,
        node.type,
        node.variables.map(visit).toList(growable: false));
  }

  void residualizeDeclarationList(scanner.Keyword keyword,
                                  List<ast.VariableDeclaration> decls) {
    if (decls.isEmpty) return;
    addStatement(AstFactory.variableDeclarationStatement2(keyword, decls));
  }

  DeclarationListTransformer translateDeclarationList(
      scanner.Keyword keyword,
      ast.VariableDeclarationList node) {
    assert(node.variables.isNotEmpty);
    return (ErrorCont f, DeclarationListCont s) {
      translateDecl(ast.VariableDeclaration decl, cont) {
        if (decl.initializer == null) {
          return cont(decl);
        } else {
          return visitExpression(decl.initializer)(f, (expr) {
            return cont(AstFactory.variableDeclaration2(decl.name.name, expr));
          });
        }
      }

      if (node.variables.length == 1) {
        translateDecl(node.variables.first, (decl) {
          return s([decl]);
        });
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
            residualizeDeclarationList(keyword, decls);
            decls.clear();
          }
          translateDecl(nextDecl, nextCont);
        };
      }
      translateDecl(node.variables.first, cont);
    };
  }

  visitVariableDeclaration(ast.VariableDeclaration node) {
    return new ast.VariableDeclaration(
        node.documentationComment,
        node.metadata,
        node.name,
        node.equals,
        visit(node.initializer));
  }

  // Statements
  StatementTransformer visitBlock(ast.Block node) {
    return (ErrorCont f, ReturnCont r, StatementCont s) {
      for (var stmt in node.statements.reversed) {
        var current = s;
        s = () {
          return visit(stmt)(f, r, current);
        };
      }
      return s();
    };
  }

  StatementTransformer visitBreakStatement(ast.BreakStatement node) {
    return (ErrorCont f, ReturnCont r, StatementCont s) {
      addStatement(
          AstFactory.functionExpressionInvocation(
              breakTargets.last, [nullLiteral()]));
    };
  }

  StatementTransformer visitContinueStatement(ast.ContinueStatement node) {
    return (ErrorCont f, ReturnCont r, StatementCont s) {
      addStatement(
          AstFactory.functionExpressionInvocation(
              continueTargets.last, [nullLiteral()]));
    };
  }

  StatementTransformer visitExpressionStatement(ast.ExpressionStatement node) {
    return (ErrorCont f, ReturnCont r, StatementCont s) {
      return visitExpression(node.expression)(f, (expr) {
        addStatement(expr);
        return s();
      });
    };
  }

  StatementTransformer visitIfStatement(ast.IfStatement node) {
    return (ErrorCont f, ReturnCont r, StatementCont s) {
      var hasElse = node.elseStatement != null;
      return visitExpression(node.condition)(f, (expr) {
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
  }

  StatementTransformer visitReturnStatement(ast.ReturnStatement node) {
    return (ErrorCont f, ReturnCont r, StatementCont s) {
      if (node.expression == null) {
        return r(AstFactory.nullLiteral());
      } else {
        return visitExpression(node.expression)(f, (v) => r(v));
      }
    };
  }

  StatementTransformer visitTryStatement(ast.TryStatement node) {
    return (ErrorCont f, ReturnCont r, StatementCont s) {
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
        AstFactory.expressionStatement(
            AstFactory.methodInvocation2(
                finallyName,
                [identifier(joinName), AstFactory.nullLiteral()]));
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
          AstFactory.tryStatement3(
              tryBlock,
              [AstFactory.catchClause(
                    name,
                    [AstFactory.expressionStatement(
                          AstFactory.methodInvocation2(catchName, [identifier(name)]))])],
              AstFactory.block(
                  [AstFactory.expressionStatement(
                        AstFactory.methodInvocation2(
                            finallyName,
                            [identifier(joinName), AstFactory.nullLiteral()]))])));
    };
  }

  StatementTransformer visitVariableDeclarationStatement(
      ast.VariableDeclarationStatement node) {
    return (ErrorCont f, ReturnCont r, StatementCont s) {
      var keyword = scanner.Keyword.keywords[node.variables.keyword.lexeme];
      return translateDeclarationList(keyword, node.variables)(f, (decls) {
        residualizeDeclarationList(keyword, decls);
        return s();
      });
    };
  }

  StatementTransformer visitWhileStatement(ast.WhileStatement node) {
    return (ErrorCont f, ReturnCont r, StatementCont s) {
      var breakName = newName('break');
      var continueName = newName('continue');

      var savedBlock = currentBlock;
      var breakBlock = currentBlock = emptyBlock();
      s();

      var continueBlock = currentBlock = emptyBlock();
      visitExpression(node.condition)(f, (expr) {
        var savedBlock = currentBlock;
        var bodyBlock = currentBlock = emptyBlock();
        breakTargets.add(identifier(breakName));
        continueTargets.add(identifier(continueName));
        visit(node.body)(f, r, () {
          addStatement(
              AstFactory.functionExpressionInvocation(
                identifier(continueName), [nullLiteral()]));
        });
        currentBlock = savedBlock;
        breakTargets.removeLast();
        continueTargets.removeLast();
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
  }
}

typedef void ExpressionCont(ast.Expression expr);
typedef void ExpressionListCont(List<ast.Expression> exprs);
typedef void ErrorCont(ast.Expression expr);

typedef void ExpressionTransformer(ErrorCont f, ExpressionCont s);
typedef void ExpressionListTransformer(ErrorCont f, ExpressionListCont s);

class AsyncExpressionTransformer extends
    ast.GeneralizingAstVisitor<ExpressionTransformer> {
  final AsyncTransformer owner;

  AsyncExpressionTransformer(this.owner);

  ast.Block get currentBlock => owner.currentBlock;
  set currentBlock(ast.Block block) {
    owner.currentBlock = block;
  }

  Set<ast.AstNode> get awaits => owner.awaits;

  visit(ast.Expression node) => node.accept(this);

  ExpressionTransformer visitNode(ast.AstNode node) {
    throw 'AsyncExpressionTransformer(${node.runtimeType})';
  }

  ast.FunctionExpression reifyExpressionCont(ExpressionCont s, ErrorCont f) {
    var savedBlock = currentBlock;
    var exnBlock = currentBlock = emptyBlock();
    var exnName = owner.newName('e');
    f(identifier(exnName));

    currentBlock = emptyBlock();
    var name = owner.newName('x');
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

  ExpressionListTransformer translateArgumentList(ast.ArgumentList node) {
    return (ErrorCont f, ExpressionListCont s) {
      if (node.arguments.isEmpty) {
        return s([]);
      }
      var args = [];
      var seenAwait = false;
      var cont = (v) {
        args.add(v);
        return s(args);
      };
      for (var i = node.arguments.length - 1; i >= 1; --i) {
        var expr = node.arguments[i];
        seenAwait = seenAwait || awaits.contains(expr);
        var current = cont;
        if (seenAwait) {
          cont = (v) {
            var value = owner.addDeclaration('v', v);
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
      return visit(node.arguments.first)(f, cont);
    };
  }

  ExpressionTransformer visitAssignmentExpression(
      ast.AssignmentExpression node) {
    assert(node.leftHandSide is ast.SimpleIdentifier);
    return (ErrorCont f, ExpressionCont s) {
      visit(node.rightHandSide)(f, (expr) {
        return s(AstFactory.assignmentExpression(
            node.leftHandSide, node.operator.type, expr));
      });
    };
  }

  ExpressionTransformer visitAwaitExpression(ast.AwaitExpression node) {
    return (ErrorCont f, ExpressionCont s) {
      if (awaits.contains(node.expression)) {
        visit(node.expression)(f, (expr) {
          owner.addStatement(
              AstFactory.methodInvocation(
                  expr,
                  'then',
                  [reifyExpressionCont(s, f),
                    AstFactory.namedExpression2('onError', owner.reifyErrorCont(f))]));
        });
      } else {
        owner.addStatement(
            AstFactory.methodInvocation(
                node.expression,
                'then',
                [reifyExpressionCont(s, f),
                  AstFactory.namedExpression2('onError', owner.reifyErrorCont(f))]));
      }
    };
  }

  ExpressionTransformer visitBinaryExpression(ast.BinaryExpression node) {
    // TODO: Is this even possible?
    assert(node.operator.lexeme != '||' && node.operator.lexeme != '&&');
    return (ErrorCont f, ExpressionCont s) {
      return visit(node.leftOperand)(f, (left) {
        if (awaits.contains(node.rightOperand)) {
          left = owner.addDeclaration('v', left);
        }
        return visit(node.rightOperand)(f, (right) {
          s(AstFactory.binaryExpression(left, node.operator.type, right));
        });
      });
    };
  }

  ExpressionTransformer visitFunctionExpression(ast.FunctionExpression node) {
    return (ErrorCont f, ExpressionCont s) {
      s(node);
    };
  }

  ExpressionTransformer visitInstanceCreationExpression(
      ast.InstanceCreationExpression node) {
    return (ErrorCont f, ExpressionCont s) {
      translateArgumentList(node.argumentList)(f, (rands) {
        s(AstFactory.instanceCreationExpression(
            scanner.Keyword.keywords[node.keyword.lexeme],
                node.constructorName,
                rands));
      });
    };
  }

  ExpressionTransformer visitIntegerLiteral(ast.IntegerLiteral node) {
    return (ErrorCont f, ExpressionCont s) {
      s(node);
    };
  }

  ExpressionTransformer visitMethodInvocation(ast.MethodInvocation node) {
    return (ErrorCont f, ExpressionCont s) {
      if (node.target != null) {
        visit(node.target)(f, (rator) {
          translateArgumentList(node.argumentList)(f, (rands) {
            s(AstFactory.methodInvocation(rator, node.methodName.name, rands));
          });
        });
      } else {
        translateArgumentList(node.argumentList)(f, (rands) {
          s(AstFactory.methodInvocation2(node.methodName.name, rands));
        });
      }
    };
  }

  ExpressionTransformer visitNullLiteral(ast.NullLiteral node) {
    return (ErrorCont f, ExpressionCont s) {
      s(node);
    };
  }

  ExpressionTransformer visitParenthesizedExpression(
        ast.ParenthesizedExpression node) {
    return (ErrorCont f, ExpressionCont s) {
      visit(node.expression)(f, s);
    };
  }

  ExpressionTransformer visitPrefixedIdentifier(ast.PrefixedIdentifier node) {
    return (ErrorCont f, ExpressionCont s) {
      s(node);
    };
  }

  ExpressionTransformer visitSimpleStringLiteral(ast.SimpleStringLiteral node) {
    return (ErrorCont f, ExpressionCont s) {
      s(node);
    };
  }

  ExpressionTransformer visitSimpleIdentifier(ast.SimpleIdentifier node) {
    return (ErrorCont f, ExpressionCont s) {
      s(node);
    };
  }

  ExpressionTransformer visitThrowExpression(ast.ThrowExpression node) {
    return (ErrorCont f, ExpressionCont s) {
      visit(node.expression)(f, (e) => f(e));
    };
  }
}
