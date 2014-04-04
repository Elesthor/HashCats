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