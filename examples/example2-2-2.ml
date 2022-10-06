#use "cc.ml"
#use "dcc.ml"
#use "transformation.ml"

let compose =
	let open CC in
		Lambda (String "A", Universe 0,
		Lambda (String "B", Pi (String "x", Var (String "A"), Universe 0),
		Lambda (String "C", Pi (String "x", Var (String "A"), Pi (String "y", App (Var (String "B"), Var (String "x")), Universe 0)),
		Lambda (String "g", Pi (String "x", Var (String "A"), Pi (String "y", App (Var (String "B"), Var (String "x")), App (App (Var (String "C"), Var (String "x")), Var (String "y")))),
		Lambda (String "f", Pi (String "x", Var (String "A"), App (Var (String "B"), Var (String "x"))),
		Lambda (String "x", Var (String "A"), 
			App (App (Var (String "g"), Var (String "x")), App (Var (String "f"), Var (String "x")))
	))))))

let t = CC.infer_type [] compose
let _ = check_type_preservation [] compose t

