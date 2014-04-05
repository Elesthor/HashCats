type mapdata = {mutable inters : int; mutable rues : int; mutable temps : int; mutable flotte : int; mutable depart : int}
type streetdata = {mutable cout : int; mutable long : int}

let glob_graph = ref (Obj.magic ())
let data = {inters = 0; rues = 0; temps = 0; flotte = 0; depart = 0}

let input file_name = 
	let file = open_in file_name in 
	let info = input_line file in 
	let (inters, rues, temps, flotte, depart) = Scanf.sscanf info "%d %d %d %d %d" (fun x y z t u-> (x,y,z,t,u)) in 
		data.inters <- inters;
		data.rues <- rues;
		data.temps <- temps;
		data.flotte <- flotte;
		data.depart <- depart;
	let mat = Array.make_matrix inters inters {cout = -1; long = -1} in 
	for i = 0 to data.inters -1 do 
		let line = input_line file in 
		ignore (line)
	done;
	for i = 0 to data.rues -1 do
		let line = input_line file in  
		let (inter1, inter2, sens, cout, long) = Scanf.sscanf line "%d %d %d %d %d" (fun x y z t u -> (x,y,z,t,u)) in
		if sens = 2 then begin
			mat.(inter1).(inter2).cout <- cout;
			mat.(inter1).(inter2).long <- long;
			mat.(inter2).(inter1).cout <- cout;
			mat.(inter2).(inter1).long <- long
		end
		else begin
			mat.(inter1).(inter2).cout <- cout;
			mat.(inter1).(inter2).long <- long
		end
	done;
	glob_graph := mat

let output l out = 
	let result = open_out out in 
	Printf.fprintf result "%i\n" data.flotte;
	let rec output_aux l' acc = 
		match l' with
		|[] -> ()
		|t::q -> Printf.fprintf result "%i\n" acc; List.iter (Printf.fprintf result "%i\n") t; output_aux q (acc+1)
	in 
	output_aux l 1

let l_accessibles s t_left = 
	let res = ref [] in 
	for i = 0 to (data.inters - 1) do 
		if !glob_graph.(s).(i).cout <> -1 && !glob_graph.(s).(i).cout <= t_left then 
			res := i::!res
	done;
	Array.of_list !res

let l_random () = 
	let rec random_aux time_left s = 
		let tab = l_accessibles s time_left in 
		let n = Array.length tab in 
		print_int time_left;
		print_endline "";
		if n = 0 then
			[s]
		else begin
			let r = Random.int n in 
			let t' = time_left - !glob_graph.(s).(tab.(r)).cout in 
			s :: (random_aux t' tab.(r))
		end
	in 
	List.rev (random_aux data.temps data.depart)
		
let random_total () = 
	let l = ref [] in 
	for i = 0 to data.flotte -1 do 
		l := l_random () :: !l 
	done;
	output !l "result.txt"


let () = 
	Random.self_init ();
	input "paris_54000.txt";
	random_total ()