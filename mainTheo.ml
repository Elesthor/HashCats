let input file_name = 
	let file = open_in file_name in 
	let info = input_line file in 
	let (lignes, colonnes) = Scanf.sscanf info "%d %d" (fun x y -> (x,y)) in
	let mat = Array.make_matrix lignes colonnes 0 in 
	for i = 0 to (lignes - 1) do
		let line = input_line file in  
		for j = 0 to (colonnes - 1) do 
			if line.[j] = '#' then 
				mat.(i).(j) <- 1
		done;
	done;
	mat

let square_sol mat =
	let n = Array.length mat in
	let m = Array.length (mat.(0)) in
	(* Printf.printf "%i %i" n m; *)
	let rec aux rs row cs col =
		(* Printf.printf "rs = %i row = %i cs = %i col = %i\n" rs row cs col; *)
		if row <= 0 || col <= 0 || rs + row >= n || cs + col >= m then
			[]
		else if row = 1 && col = 1 then begin
			if mat.(rs).(cs) = 1 then [(Printf.sprintf "PAINTSQ %i %i 0\n" rs cs)]
			else []
		end
		else begin
			let sum = ref 0 in
			let whites = ref [] in
			for i = rs to rs + row do
				for j = cs to cs + col do
					if mat.(i).(j) = 1 then incr sum
					else whites := (Printf.sprintf "ERASECELL %i %i\n" i j) :: (!whites)
				done
			done;
			if !sum >  3 * row*col / 4 then begin
				let size' = min (row+1) (col+1) in
				let size  = if size' mod 2 = 0 then size' - 1 else size' in
				let (center_i, center_j) = (rs + size/2, cs + size/2) in
				if size/2 <> 0 then begin
				(* if rs + size/2 >= n || cs + size >= m || rs - size < 0 || cs - size < 0 then Printf.fprintf stderr "Error !"; *)
				let l = (Printf.sprintf "PAINTSQ %i %i %i\n" center_i center_j (size/2)) :: !whites in
				let l' = aux (rs + size + 1) (row - size - 1) cs col in
				let l2 = aux rs size (cs + size + 1) (col - size - 1) in
				l @ l' @ l2
				end
			else []
			end
			else begin
				let ltl = aux rs (row/2) cs (col/2)
				and ltr = aux rs (row/2) (cs + col/2 + 1) (col - col/2)
				and lbl = aux (rs + (row/2) + 1) (row - row/2) cs (col/2)
				and lbr = aux (rs + (row/2) + 1) (row - row/2) (cs + col/2 + 1) (col - col/2) in
				ltl @ ltr @ lbl @ lbr
			end
		end
	in let list = (aux 0 (n-1) 0 (m-1)) in
	let result = ref (Printf.sprintf "%i\n" (List.length list)) in
	List.iter (fun s -> result := (!result) ^ s) list;
	Printf.printf "%s" (!result)

let () =
	let mat = input "doodle.txt" in
	square_sol mat