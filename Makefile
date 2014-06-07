translate: sexp.cmo ast.cmo ir.cmo translate.ml
	ocamlc -o translate sexp.cmo ast.cmo ir.cmo translate.ml

sexp.cmo: sexp.mli sexp.ml
	ocamlc -c sexp.mli sexp.ml

ast.cmo: ast.mli ast.ml
	ocamlc -c ast.mli ast.ml

ir.cmo: ir.mli ir.ml
	ocamlc -c ir.mli ir.ml
