(* Algo naif *)

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

(* Pour la voiture k *)
let naive k marque =
	let m = !glob_graph in
	let temps_restant = ref data.temps in
	let etapes = ref [data.depart] in
	let find_road i =
		let rec aux j =
			if j = data.rues then None
			else if (not marque.(i).(j)) || m.(i).(j).cout <> -1 then begin
				if !temps_restant >= m.(i).(j).cout then begin
					Some(j)
				end
				else aux (Random.int data.rues)
			end
			else aux (Random.int data.rues)
		in aux 0
	in
	let i = ref data.depart in
	let j = ref (find_road (!i)) in
	while !j <> None do
		match !j with
		| Some(j') ->
			begin
				etapes := j' :: (!etapes);
				marque.(!i).(j') <- true;
				i := j';
				j := find_road (!i)
			end
		| _ -> failwith "Merde"
	done;
	Printf.printf "%i\n" (List.length !etapes);
	List.iter (fun i -> Printf.printf "%i\n" i) !etapes


let () = 
	input "paris_54000.txt";
	Random.self_init();
	let marque = Array.make_matrix data.rues data.rues false in
	for k = 0 to 7 do
		naive k marque
	done