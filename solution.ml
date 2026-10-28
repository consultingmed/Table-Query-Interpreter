type column = string

type row = Eval.value list

type table = column list * row list

type query = 
	| Row_num of Ast.ident
	| Filter of Ast.expr
	| Drop of Ast.ident
	| Add_c of Ast.ident * Ast.expr
	| Limit of int
	| Cmp of query * query
	| Skip


let qtoeval e ro xss =
	let rec it env row xs =
		match row, xs with
		| r::row', x::xs' -> it (Eval.Env.add x r env) row' xs'
		| [], [] -> env
		| _, _ -> failwith "error"
	in Eval.eval (it Eval.Env.empty ro xss) e

let rec eval (q : query) (t : table) : table = 
	match q with 
	
	| Row_num x ->
		
		let rec it rows ind =
			(match rows with 
			| [] -> [] 
			| r::rows' -> ( (Eval.VInt ind)::r )::(it rows' (ind + 1)))
		in let (tabs, tabr) = t
		in (tabs @ [x]), (it tabr 0) 
		
	
	| Filter e -> 
		(
		let (tabs, tabr) = t 
		in let rec it rows = 
			match rows with
			| [] -> []
			| r::rows' -> 
				(match qtoeval e r tabs with
				| Eval.VBool true -> r::(it rows') 
				| Eval.VBool false -> it rows'
				| _ -> failwith "error")
		in tabs, (it tabr)
		)
		
	
	
	| Drop name -> 
		(
		let rec find_ind xs ind =
			match xs with
			| [] -> failwith "can't drop nonexisting column" 
			| x::xs' -> (
				if x = name then ind 	
				else find_ind xs' (ind + 1)
				)
		in let rec remove xs ind ob =
			match xs with
			| [] -> xs
			| x::xs' -> (
				if ob = ind then xs'Summary: 1 passed, 1 total


				else x::(remove xs' ind (ob + 1))  
				)
		in let rec it rows ind =
			match rows with 
			| [] -> [] 
			| r::rows' -> (remove r ind 0)::(it rows' ind)
		in let (tabs, tabr) = t
		in let i = (find_ind tabs 0) 
		in (remove tabs i 0), (it tabr i) 
		)
	
	
	| Add_c (x, e) ->
		(
		let (tabs, tabr) = t
		in let rec it rows =
			match rows with 
			| [] -> [] 
			| r::rows' -> ((qtoeval e r tabs)::r)::(it rows')
		in (tabs @ [x]), (it tabr) 
		)

	| Limit i -> 
		(
		let rec it rows ind = 
			match rows, ind with
			| _, 0 -> []
			| [], _ -> []
			| r::rows', _ -> r::(it rows' (ind - 1)) 
		in let (tabs, tabr) = t 
		in tabs, (it tabr i)
		)
		
	| Cmp (p, q) -> eval q ( eval p t )
	
	
	| Skip -> t
