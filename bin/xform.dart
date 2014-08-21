import 'package:analyzer/src/generated/ast.dart' as ast;
import 'package:analyzer/src/generated/scanner.dart' as scanner;
import 'package:analyzer/src/generated/testing/ast_factory.dart';
// import 'package:analyzer/src/generated/testing/token_factory.dart';

class AnalysisVisitor extends ast.GeneralizingAstVisitor {
  Set<ast.AstNode> awaits = new Set<ast.AstNode>();

  bool maybeAdd(ast.AstNode node, bool shouldAdd) {
    if (shouldAdd) awaits.add(node);
    return shouldAdd;
  }

  visit(ast.AstNode node) => node.accept(this);

  visitNode(ast.AstNode node) {
    throw 'TODO(${node.runtimeType})';
  }

  visitCompilationUnit(ast.CompilationUnit node) {
    node.declarations.forEach(visit);
  }

  visitFunctionDeclaration(ast.FunctionDeclaration node) {
    visit(node.functionExpression.body);
  }

  visitBlockFunctionBody(ast.BlockFunctionBody node) {
    if (node.keyword == null) return;
    if (node.star != null) {
      throw 'TODO(${node.keyword}${node.star})';
    }
    visit(node.block);
  }

  visitArgumentList(ast.ArgumentList node) {
    var result = false;
    node.arguments.forEach((e) { result = visit(e) || result; });
    return maybeAdd(node, result);
  }

  visitCatchClause(ast.CatchClause node) {
    assert(node.onKeyword == null);
    assert(node.stackTraceParameter == null);
    return maybeAdd(node, visit(node.body));
  }

  visitVariableDeclarationList(ast.VariableDeclarationList node) {
    var result = false;
    node.variables.forEach((d) { result = visit(d) || result; });
    return maybeAdd(node, result);
  }

  visitVariableDeclaration(ast.VariableDeclaration node) {
    return maybeAdd(node, visit(node.initializer));
  }

  // Statements
  visitBlock(ast.Block node) {
    var result = false;
    node.statements.forEach((s) { result = visit(s) || result; });
    return maybeAdd(node, result);
  }

  visitExpressionStatement(ast.ExpressionStatement node) {
    return maybeAdd(node, visit(node.expression));
  }

  visitIfStatement(ast.IfStatement node) {
    var result = visit(node.condition);
    result = visit(node.thenStatement) || result;
    if (node.elseStatement != null) {
      result = visit(node.elseStatement) || result;
    }
    return maybeAdd(node, result);
  }

  visitTryStatement(ast.TryStatement node) {
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

  visitVariableDeclarationStatement(ast.VariableDeclarationStatement node) {
    return maybeAdd(node, visit(node.variables));
  }

  // Expressions
  visitSimpleStringLiteral(ast.SimpleStringLiteral node) {
    return false;
  }

  visitSimpleIdentifier(ast.SimpleIdentifier node) {
    return false;
  }

  visitMethodInvocation(ast.MethodInvocation node) {
    var result = node.target != null && visit(node.target);
    result = visit(node.argumentList) || result;
    if (result) awaits.add(node);
    return result;
  }

  visitAwaitExpression(ast.AwaitExpression node) {
    visit(node.expression);
    awaits.add(node);
    return true;
  }

  visitFunctionExpression(ast.FunctionExpression node) {
    visit(node.body);
    return false;
  }
}

class TransformVisitor extends ast.GeneralizingAstVisitor {
  final Set<ast.AstNode> awaits;

  ast.Block currentBlock;

  TransformVisitor(this.awaits);

  visit(ast.AstNode node) => node.accept(this);

  int counter = 0;

  // TODO: Safely generate fresh identifiers.
  String newName(String base) => '$base${counter++}';

  /// Insert a declaration with initial value [expr] in [currentBlock] and
  /// return the fresh name.
  ///
  /// No declaration is added if the expression is already a value and [force]
  /// is false.
  ast.Expression addDeclaration(String base,
                                ast.Expression expr,
                                {force: false}) {
    if (!force &&
        ((expr is ast.SimpleIdentifier ||
          expr is ast.SimpleStringLiteral))) {
      return expr;
    }
    var name = newName(base);
    currentBlock.statements.add(
        makeVariableDeclarationStatement(name, expr));
    return makeIdentifier(name);
  }

  visitNode(ast.AstNode node) {
    throw 'TODO(${node.runtimeType})';
  }

  static ast.SimpleIdentifier makeIdentifier(String name) {
    return AstFactory.identifier3(name);
  }

  static ast.VariableDeclarationStatement makeVariableDeclarationStatement(
      String name, ast.Expression initializer) {
    return AstFactory.variableDeclarationStatement2(
        scanner.Keyword.VAR,
        [AstFactory.variableDeclaration2(name, initializer)]);
  }

  reifyExpressionCont(s, f) {
    var savedBlock = currentBlock;
    var exnBlock = currentBlock = AstFactory.block([]);
    String exnName = newName('e');
    f(makeIdentifier(exnName));

    currentBlock = AstFactory.block([]);
    String name = newName('x');
    s(makeIdentifier(name));

    var fun = AstFactory.functionExpression2(
        AstFactory.formalParameterList([AstFactory.simpleFormalParameter3(name)]),
        AstFactory.blockFunctionBody(
            AstFactory.block(
                [AstFactory.tryStatement2(
                   currentBlock,
                   [AstFactory.catchClause(exnName, exnBlock.statements)])])));
    currentBlock = savedBlock;
    return fun;
  }

  reifyErrorCont(f) {
    var savedBlock = currentBlock;
    currentBlock = AstFactory.block([]);
    String name = newName('e');
    f(makeIdentifier(name));
    var fun = AstFactory.functionExpression2(
        AstFactory.formalParameterList([AstFactory.simpleFormalParameter3(name)]),
        AstFactory.blockFunctionBody(currentBlock));
    currentBlock = savedBlock;
    return fun;
  }

  reifyStatementCont(s) {
    var savedBlock = currentBlock;
    currentBlock = AstFactory.block([]);
    s();
    var fun = AstFactory.functionExpression2(
        AstFactory.formalParameterList([]),
        AstFactory.blockFunctionBody(currentBlock));
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
    currentBlock = AstFactory.block([]);
    var result = newName('result');
    visit(node.block)(
        (e) { currentBlock.statements.add(
                 AstFactory.expressionStatement(
                     AstFactory.methodInvocation(
                       makeIdentifier(result),
                       'completeError',
                       [e]))); },
        (v) { throw 'returned'; },
        () { currentBlock.statements.add(
                 AstFactory.expressionStatement(
                     AstFactory.methodInvocation(
                         makeIdentifier(result),
                         'complete',
                         [AstFactory.nullLiteral()]))); });

    String exnName = newName('e');
    return AstFactory.blockFunctionBody2(
        [makeVariableDeclarationStatement(result,
            AstFactory.instanceCreationExpression2(
                scanner.Keyword.NEW,
                AstFactory.typeName4('Completer', []),
                [])),
         AstFactory.expressionStatement(
             AstFactory.methodInvocation2(
                 'scheduleMicrotask',
                 [AstFactory.functionExpression2(AstFactory.formalParameterList([]),
                      AstFactory.blockFunctionBody(
                          AstFactory.block([
                          AstFactory.tryStatement2(
                              currentBlock,
                              [AstFactory.catchClause(
                                   exnName,
                                   [AstFactory.expressionStatement(
                                       AstFactory.methodInvocation(
                                         makeIdentifier(result),
                                         'completeError',
                                         [makeIdentifier(exnName)]))])])])))])),
         AstFactory.returnStatement2(AstFactory.propertyAccess2(makeIdentifier(result), 'future'))]);
  }

  visitArgumentList(ast.ArgumentList node) => (f, s) {
    if (node.arguments.isEmpty) {
      s([]);
    } else {
      var args = [];
      var seenAwait = false;
      var cont = (v) { args.add(v); return s(args); };
      for (var i = node.arguments.length - 1; i >= 1; --i) {
        var expr = node.arguments[i];
        seenAwait = seenAwait || awaits.contains(expr);
        var current = cont;
        if (seenAwait) {
          cont = (v) {
            var value = addDeclaration('v', v);
            args.add(value);
            return visit(expr)(f, current);
          };
        } else {
          cont = (v) {
            args.add(v);
            return visit(expr)(f, current);
          };
        }
      }
      return visit(node.arguments.first)(f, cont);
    }
  };

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
  visitBlock(ast.Block node) => (f, r, s) {
    if (!awaits.contains(node)) {
      currentBlock.statements.add(node);
      return s();
    }
    for (var stmt in node.statements.reversed) {
      var current = s;
      s = () { return visit(stmt)(f, r, current); };
    }
    return s();
  };

  visitExpressionStatement(ast.ExpressionStatement node) => (f, r, s) {
    if (!awaits.contains(node)) {
      currentBlock.statements.add(node);
      return s();
    }
    return visit(node.expression)(f, (expr) {
        currentBlock.statements.add(AstFactory.expressionStatement(expr));
        return s();
    });
  };

  visitIfStatement(ast.IfStatement node) => (f, r, s) {
    if (!awaits.contains(node)) {
      currentBlock.statements.add(node);
      return s();
    }
    var hasElse = node.elseStatement != null;
    if (awaits.contains(node.condition)) {
      return visit(node.condition)(f, (expr) {
        if (!awaits.contains(node.thenStatement) &&
            (!hasElse || !awaits.contains(node.elseStatement))) {
          currentBlock.statements.add(
              AstFactory.ifStatement2(expr,
                                      node.thenStatement,
                                      node.elseStatement));
          return s();
        }

        var joinName = newName('join');
        var joinFun = reifyStatementCont(s);
        currentBlock.statements.add(AstFactory.functionDeclarationStatement(
            null, null, joinName, joinFun));
        s = () {
          currentBlock.statements.add(
              AstFactory.expressionStatement(
                  AstFactory.methodInvocation2(joinName, [])));
        };
        var savedBlock = currentBlock;
        var thenBlock = currentBlock = AstFactory.block([]);
        visit(node.thenStatement)(f, r, s);
        var elseBlock = currentBlock = AstFactory.block([]);;
        if (hasElse) {
          visit(node.elseStatement)(f, r, s);
        } else {
          s();
        }
        currentBlock = savedBlock;
        currentBlock.statements.add(
            AstFactory.ifStatement2(expr, thenBlock, elseBlock));
      });
    } else {
      var joinName = newName('join');
      var joinFun = reifyStatementCont(s);
      currentBlock.statements.add(AstFactory.functionDeclarationStatement(
          null, null, joinName, joinFun));
      s = () {
        currentBlock.statements.add(
            AstFactory.expressionStatement(
                AstFactory.methodInvocation2(joinName, [])));
      };
      var savedBlock = currentBlock;
      var thenBlock = currentBlock = AstFactory.block([]);
      visit(node.thenStatement)(f, r, s);
      var elseBlock = currentBlock = AstFactory.block([]);;
      if (hasElse) {
        visit(node.elseStatement)(f, r, s);
      } else {
        s();
      }
      currentBlock = savedBlock;
      currentBlock.statements.add(
          AstFactory.ifStatement2(node.condition, thenBlock, elseBlock));
    }
  };

  visitTryStatement(ast.TryStatement node) => (f, r, s) {
    if (!awaits.contains(node)) {
      currentBlock.statements.add(node);
      return s();
    }
    var joinName = newName('join');
    ast.FunctionExpression joinFun = reifyStatementCont(s);
    joinFun.parameters =
        AstFactory.formalParameterList([AstFactory.simpleFormalParameter3('_')]);
    currentBlock.statements.add(AstFactory.functionDeclarationStatement(
      null, null, joinName, joinFun));// TODO: add arg to joinFun.

    var finallyName = newName('finally');
    var finallyContName = newName('cont');
    var finallyValueName = newName('v');
    var savedBlock = currentBlock;
    var finallyBlock = currentBlock = AstFactory.block([]);
    s = () {
      currentBlock.statements.add(
          AstFactory.expressionStatement(
            AstFactory.functionExpressionInvocation(
              makeIdentifier(finallyContName),
              [makeIdentifier(finallyValueName)])));
    };
    if (node.finallyBlock != null) {
      visit(node.finallyBlock)(f, r, s);
    } else {
      s();
    }

    var catchName = newName('catch');
    var exnName = node.catchClauses.isEmpty
        ? newName('e')
        : node.catchClauses.first.exceptionParameter.name;
    var catchBlock = currentBlock = AstFactory.block([]);
    if (node.catchClauses.isNotEmpty) {
      assert(node.catchClauses.length == 1);
      visit(node.catchClauses.first)(
          (e) {
            currentBlock.statements.add(
                AstFactory.expressionStatement(
                  AstFactory.methodInvocation2(
                      finallyName,
                      [reifyErrorCont(f), e])));
          },
          (v) {
            currentBlock.statements.add(
                AstFactory.expressionStatement(
                  AstFactory.methodInvocation2(
                      finallyName,
                      [reifyErrorCont(r), v])));
          },
          () {
            currentBlock.statements.add(
              AstFactory.expressionStatement(
                AstFactory.methodInvocation2(
                    finallyName,
                    [makeIdentifier(joinName),
                     AstFactory.nullLiteral()])));
          });
    } else {
      currentBlock.statements.add(
          AstFactory.expressionStatement(
            AstFactory.methodInvocation2(
                finallyName,
                [makeIdentifier(joinName),
                 AstFactory.nullLiteral()])));
    }

    var tryBlock = currentBlock = AstFactory.block([]);
    visit(node.body)(
        (e) {
          currentBlock.statements.add(
              AstFactory.expressionStatement(
                AstFactory.methodInvocation2(catchName, [e])));
        },
        (v) {
          currentBlock.statements.add(
              AstFactory.expressionStatement(
                AstFactory.methodInvocation2(
                    finallyName,
                    [reifyErrorCont(r), v])));
        },
        () {
          AstFactory.expressionStatement(
            AstFactory.methodInvocation2(
                finallyName,
                [makeIdentifier(joinName),
                 AstFactory.nullLiteral()]));
        });

    currentBlock = savedBlock;
    currentBlock.statements.add(
        AstFactory.functionDeclarationStatement(
            null,
            null,
            finallyName,
            AstFactory.functionExpression2(
                AstFactory.formalParameterList(
                    [AstFactory.simpleFormalParameter3(finallyContName),
                     AstFactory.simpleFormalParameter3(finallyValueName)]),
                AstFactory.blockFunctionBody(finallyBlock))));

    currentBlock.statements.add(
        AstFactory.functionDeclarationStatement(
            null,
            null,
            catchName,
            AstFactory.functionExpression2(
                AstFactory.formalParameterList(
                    [AstFactory.simpleFormalParameter3(exnName)]),
                AstFactory.blockFunctionBody(catchBlock))));
     var name = newName('e');
     currentBlock.statements.add(
        AstFactory.tryStatement3(
            tryBlock,
            [AstFactory.catchClause(name,
                 [AstFactory.expressionStatement(
                       AstFactory.methodInvocation2(catchName,
                                                    [makeIdentifier(name)]))])],
            AstFactory.block([
                AstFactory.expressionStatement(
                    AstFactory.methodInvocation2(
                        finallyName,
                        [makeIdentifier(joinName),
                         AstFactory.nullLiteral()]))])));
  };

  visitVariableDeclarationStatement(ast.VariableDeclarationStatement node) =>
  (f, r, s) {
    if (!awaits.contains(node)) {
      currentBlock.statements.add(node);
      return s();
    }
    assert(node.variables.variables.length == 1);
    var variable = node.variables.variables.first;
    assert(variable.initializer != null);
    return visit(variable.initializer)(f, (expr) {
      addDeclaration(variable.name.name, expr, force: true);
      return s();
    });
  };

  // Expressions
  visitSimpleStringLiteral(ast.SimpleStringLiteral node) => (f, s) {
    return s(node);
  };

  visitSimpleIdentifier(ast.SimpleIdentifier node) => (f, s) {
    return s(node);
  };

  visitMethodInvocation(ast.MethodInvocation node) => (f, s) {
    if (node.target != null) {
      return visit(node.target)(f, (rator) {
        return visit(node.argumentList)(f, (rands) {
          return s(AstFactory.methodInvocation(
              rator,
              node.methodName.name,
              rands));
        });
      });
    } else {
      return visit(node.argumentList)(f, (rands) {
        return s(AstFactory.methodInvocation2(
            node.methodName.name,
            rands));
      });
    }
  };

  visitAwaitExpression(ast.AwaitExpression node) => (f, s) {
    if (awaits.contains(node.expression)) {
      return visit(node.expression)(f, (expr) {
          currentBlock.statements.add(
              AstFactory.expressionStatement(
                  AstFactory.methodInvocation(
                      expr,
                      'then',
                      [reifyExpressionCont(s, f),
                       AstFactory.namedExpression2('onError',
                                                   reifyErrorCont(f))])));
      });
    } else {
      currentBlock.statements.add(
          AstFactory.expressionStatement(
            AstFactory.methodInvocation(
                node.expression,
                'then',
                [reifyExpressionCont(s, f),
                 AstFactory.namedExpression2('onError',
                                             reifyErrorCont(f))])));
    }
  };

  visitFunctionExpression(ast.FunctionExpression node) => (f, s) {
    return s(node);
  };
}
