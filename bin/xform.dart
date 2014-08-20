import 'package:analyzer/src/generated/ast.dart' as ast;
import 'package:analyzer/src/generated/scanner.dart' as scanner;
import 'package:analyzer/src/generated/testing/ast_factory.dart';
// import 'package:analyzer/src/generated/testing/token_factory.dart';

class AnalysisVisitor extends ast.GeneralizingAstVisitor {
  Set<ast.AstNode> awaits = new Set<ast.AstNode>();
  
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
    if (result) awaits.add(node);
    return result;
  }
  
  visitVariableDeclarationList(ast.VariableDeclarationList node) {
    node.variables.forEach(visit);
  }
  
  visitVariableDeclaration(ast.VariableDeclaration node) {
    visit(node.initializer);
  }

  // Statements
  visitBlock(ast.Block node) {
    node.statements.forEach(visit);
  }
  
  visitExpressionStatement(ast.ExpressionStatement node) {
    visit(node.expression);
  }
  
  visitVariableDeclarationStatement(ast.VariableDeclarationStatement node) {
    visit(node.variables);
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
  
  reify(s) {
    var savedBlock = currentBlock;
    currentBlock = AstFactory.block([]);
    String name = newName('x');
    s(makeIdentifier(name));
    var fun = AstFactory.functionExpression2(
        AstFactory.formalParameterList([AstFactory.simpleFormalParameter3(name)]),
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
        () { throw 'returned'; },
        () { currentBlock.statements.add(
                 AstFactory.expressionStatement(
                     AstFactory.methodInvocation(
                         makeIdentifier(result), 
                         'complete', 
                         [AstFactory.nullLiteral()]))); });
    
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
                      AstFactory.blockFunctionBody(currentBlock))])),
         AstFactory.returnStatement2(AstFactory.propertyAccess2(makeIdentifier(result), 'future'))]);
  }
  
  visitArgumentList(ast.ArgumentList node) => (s) {
    if (node.arguments.isEmpty) {
      s([]);
    } else {
      var args = [];
      var cont = (v) { args.add(v); return s(args); };
      for (var expr in node.arguments.skip(1).toList().reversed) {
        var current = cont;
        cont = (v) { args.add(v); return visit(expr)(current); };
      }
      return visit(node.arguments.first)(cont);
    }
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
  visitBlock(ast.Block node) => (r, s) {
    for (var stmt in node.statements.reversed) {
      var current = s;
      s = () { return visit(stmt)(r, current); };
    }
    return s();
  };
  
  visitExpressionStatement(ast.ExpressionStatement node) => (r, s) {
    if (awaits.contains(node.expression)) {
      return visit(node.expression)((_) => s());
    } else {
      currentBlock.statements.add(node);
      return s();
    }
  };
  
  visitVariableDeclarationStatement(ast.VariableDeclarationStatement node) => (r, s) {
    assert(node.variables.variables.length == 1);
    assert(node.variables.variables.first.initializer != null);
    return visit(node.variables.variables.first.initializer)((expr) {
      currentBlock.statements.add(makeVariableDeclarationStatement(
          node.variables.variables.first.name.name,
          expr));
      return s();
    });
  };
  
  // Expressions
  visitSimpleStringLiteral(ast.SimpleStringLiteral node) => (s) {
    return s(node);
  };
  
  visitSimpleIdentifier(ast.SimpleIdentifier node) => (s) {
    return s(node);
  };
  
  visitMethodInvocation(ast.MethodInvocation node) => (s) {
    if (node.target != null) {
      return visit(node.target)((rator) {
        return visit(node.argumentList)((rands) {
          String name = newName('x');
          ast.VariableDeclarationStatement decl =
              makeVariableDeclarationStatement(name,
                  AstFactory.methodInvocation(
                    rator,
                    node.methodName.name,
                    rands));
          currentBlock.statements.add(decl);
          return s(makeIdentifier(name));
        });
      });
    } else {
      return visit(node.argumentList)((rands) {
        String name = newName('x');
        ast.VariableDeclarationStatement decl =
            makeVariableDeclarationStatement(name,
                AstFactory.methodInvocation2(
                    node.methodName.name,
                    rands));
        currentBlock.statements.add(decl);
        return s(makeIdentifier(name));
      });
    }
  };

  visitAwaitExpression(ast.AwaitExpression node) => (s) {
    if (awaits.contains(node.expression)) {
      return visit(node.expression)((expr) {
          currentBlock.statements.add(
              AstFactory.expressionStatement(
                  AstFactory.methodInvocation(
                      expr,
                      'then',
                      [reify(s)])));
      });
    } else {
      currentBlock.statements.add(
          AstFactory.expressionStatement(
            AstFactory.methodInvocation(
                node.expression,
                'then',
                [reify(s)])));
    }
  };

  visitFunctionExpression(ast.FunctionExpression node) => (s) {
    return s(node);
  };
}
