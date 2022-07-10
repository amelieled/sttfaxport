type error = [ `CompileDefnWithoutType | `CompileCommand | `CompileRewriteRule ]

val compile_entry :
  Api.Env.t ->
  Parsers.Entry.entry ->
  (Ast.item, [> error | Compile_proof.error | Compile_term.error ]) result
