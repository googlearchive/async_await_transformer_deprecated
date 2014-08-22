import 'package:analyzer/src/generated/ast.dart' as ast;
import 'package:analyzer/src/generated/scanner.dart' as scanner;
import 'package:analyzer/src/generated/testing/ast_factory.dart';

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
  bool visitAwaitExpression(ast.AwaitExpression node) {
    visit(node.expression);
    awaits.add(node);
    return true;
  }

  bool visitFunctionExpression(ast.FunctionExpression node) {
    visit(node.body);
    return false;
  }

  bool visitMethodInvocation(ast.MethodInvocation node) {
    var result = node.target != null && visit(node.target);
    result = visit(node.argumentList) || result;
    return maybeAdd(node, result);
  }

  bool visitParenthesizedExpression(ast.ParenthesizedExpression node) {
    return maybeAdd(node, visit(node.expression));
  }

  bool visitSimpleStringLiteral(ast.SimpleStringLiteral node) {
    return false;
  }

  bool visitSimpleIdentifier(ast.SimpleIdentifier node) {
    return false;
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

typedef void StatementCont();
typedef void ReturnCont(ast.Expression expr);

typedef void StatementTransformer(ErrorCont f, ReturnCont r, StatementCont s);

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
    var result = newName('result');
    visit(node.block)((e) {
      addStatement(AstFactory.methodInvocation(identifier(result),
                                               'completeError',
                                               [e]));
    }, (v) {
      throw 'returned';
    }, () {
      addStatement(AstFactory.methodInvocation(
          identifier(result),
          'complete',
          [AstFactory.nullLiteral()]));
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
      if (!awaits.contains(node)) {
        addStatement(node);
        return s();
      }
      for (var stmt in node.statements.reversed) {
        var current = s;
        s = () {
          return visit(stmt)(f, r, current);
        };
      }
      return s();
    };
  }

  StatementTransformer visitExpressionStatement(ast.ExpressionStatement node) {
    return (ErrorCont f, ReturnCont r, StatementCont s) {
      if (!awaits.contains(node)) {
        addStatement(node);
        return s();
      }
      return visitExpression(node.expression)(f, (expr) {
        addStatement(expr);
        return s();
      });
    };
  }

  StatementTransformer visitIfStatement(ast.IfStatement node) {
    return (ErrorCont f, ReturnCont r, StatementCont s) {
      if (!awaits.contains(node)) {
        addStatement(node);
        return s();
      }
      var hasElse = node.elseStatement != null;
      if (awaits.contains(node.condition)) {
        return visitExpression(node.condition)(f, (expr) {
          if (!awaits.contains(node.thenStatement) &&
              (!hasElse || !awaits.contains(node.elseStatement))) {
            addStatement(
                AstFactory.ifStatement2(expr,
                                        node.thenStatement,
                                        node.elseStatement));
            return s();
          }

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
      } else {
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
        addStatement(AstFactory.ifStatement2(node.condition,
                                             thenBlock,
                                             elseBlock));
      }
    };
  }

  StatementTransformer visitTryStatement(ast.TryStatement node) {
    return (ErrorCont f, ReturnCont r, StatementCont s) {
      if (!awaits.contains(node)) {
        addStatement(node);
        return s();
      }
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
      if (!awaits.contains(node)) {
        addStatement(node);
        return s();
      }
      assert(node.variables.variables.length == 1);
      var variable = node.variables.variables.first;
      assert(variable.initializer != null);
      return visitExpression(variable.initializer)(f, (expr) {
        addDeclaration(variable.name.name, expr, force: true);
        return s();
      });
    };
  }

  StatementTransformer visitWhileStatement(ast.WhileStatement node) {
    return (ErrorCont f, ReturnCont r, StatementCont s) {
      if (!awaits.contains(node)) {
        addStatement(node);
        return s();
      }

      var loopName = newName('loop');
      var savedBlock = currentBlock;
      var loopBlock = currentBlock = emptyBlock();
      visitExpression(node.condition)(f, (expr) {
        var savedBlock = currentBlock;
        var thenBlock = currentBlock = emptyBlock();
        visit(node.body)(f, r, () {
          addStatement(
              AstFactory.methodInvocation(
                  AstFactory.methodInvocation(
                      identifier('Future'),
                      'wait',
                      [AstFactory.listLiteral([])]),
                  'then',
                  [functionExpression(['_'], AstFactory.methodInvocation2(loopName, []))]));
        });
        var elseBlock = currentBlock = emptyBlock();
        s();
        currentBlock = savedBlock;
        addStatement(
            AstFactory.ifStatement2(expr, thenBlock, elseBlock));
      });
      currentBlock = savedBlock;
      addStatement(
          AstFactory.functionDeclarationStatement(
              null,
              null,
              loopName,
              functionExpression([], loopBlock)));
      addStatement(AstFactory.methodInvocation2(loopName, []));
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

  ExpressionTransformer visitFunctionExpression(ast.FunctionExpression node) {
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

  ExpressionTransformer
      visitParenthesizedExpression(ast.ParenthesizedExpression node) {
    return (ErrorCont f, ExpressionCont s) {
      visit(node.expression)(f, s);
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
}
