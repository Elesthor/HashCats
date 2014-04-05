type mapdata = {mutable inters : int; mutable rues : int; mutable temps : int; mutable flotte : int; mutable depart : int}
type streetdata = {mutable cout : int; mutable long : int}

let glob_graph = ref (Obj.magic ())
let glob_gps = ref (Obj.magic ())
let data = {inters = 0; rues = 0; temps = 0; flotte = 0; depart = 0}

let score = ref 0
let score_prec = ref (-1)
let solutions = ref 0
let glob_timeleft = ref 0
let glob_zerogps = ref (0., 0.)

let deg2rad (deg : float) : float =
	deg *. 3.14 /. 180.

let input file_name = 
	let file = open_in file_name in 
	let info = input_line file in 
	let (inters, rues, temps, flotte, depart) = Scanf.sscanf info "%d %d %d %d %d" (fun x y z t u-> (x,y,z,t,u)) in 
		data.inters <- inters;
		data.rues <- rues;
		data.temps <- temps;
		data.flotte <- flotte;
		data.depart <- depart;
	let mat = Array.make_matrix inters inters (-1,-1) in
	let gps = Array.make inters (0.,0.) in
	for i = 0 to data.inters -1 do 
		let line = input_line file in 
		let (lat, long) = Scanf.sscanf line "%f %f" (fun x y -> (x,y)) in 
		gps.(i) <- (lat,long)
	done;
	glob_zerogps := gps.(data.depart);
	glob_gps := gps;
	for i = 0 to data.rues -1 do
		let line = input_line file in  
		let (inter1, inter2, sens, cout, long) = Scanf.sscanf line "%d %d %d %d %d" (fun x y z t u -> (x,y,z,t,u)) in
		if sens = 2 then begin
			mat.(inter1).(inter2) <- (cout, long);
			mat.(inter2).(inter1) <- (cout, long)
		end
		else
			mat.(inter1).(inter2) <- (cout, long)
	done;
	glob_graph := mat

let center_gps () = 
	for i = 0 to data.inters - 1 do 
		!glob_gps.(i) <- ((fst !glob_gps.(i)) -. (fst !glob_gps.(data.depart)), (snd !glob_gps.(i)) -. (snd !glob_gps.(data.depart)))
	done

let output l out = 
	let result = open_out out in 
	Printf.fprintf result "%i\n" data.flotte;
	let rec output_aux l' = 
		match l' with
		|[] -> ()
		|t::q -> Printf.fprintf result "%i\n" (List.length t); 
				 List.iter (Printf.fprintf result "%i\n") t; 
				 output_aux q 
	in 
	output_aux l;
	close_out result

let deplacer i j = 
	(* Printf.printf "deplacer %i %i\n" i j; *)
	glob_timeleft := !glob_timeleft - (fst !glob_graph.(i).(j));
	if snd !glob_graph.(i).(j) = -1 then
		failwith "deplacement impossible"
	else if (snd !glob_graph.(i).(j)) <> -2 then begin
		score := !score + (snd !glob_graph.(i).(j));
		!glob_graph.(i).(j) <- (fst !glob_graph.(i).(j), -2)
	end
	else 
		()

let l_pureaccessibles s t_left = 
	let res = ref [] in 
	for i = 0 to (data.inters - 1) do 
		if (fst !glob_graph.(s).(i)) <> -1 && (snd !glob_graph.(s).(i)) <> -2 && (fst !glob_graph.(s).(i)) <= t_left then 
			res := i::!res
	done;
	Array.of_list !res

let l_noaccessibles s t_left = 
	let res = ref [] in 
	for i = 0 to (data.inters - 1) do 
		if (fst !glob_graph.(s).(i)) <> -1 && (fst !glob_graph.(s).(i)) <= t_left then 
			res := i::!res
	done;
	Array.of_list !res

let l_accessibles s = 
	let t = l_pureaccessibles s !glob_timeleft in 
	if Array.length t <> 0 then t 
	else l_noaccessibles s !glob_timeleft

let maximal f = 
	let max_i = ref 0 in 
	let max_elt = ref !glob_gps.(0) in 
	for i = 0 to data.inters -1 do 
		if f !max_elt !glob_gps.(i) then begin
			max_i := i;
			max_elt := !glob_gps.(i)
		end
	done;
	!max_i

let abs_float f = if f < 0. then (-.f) else f

let etoile () = 
	let f1 (a,b) (c,d) = (b <= d) && ((abs_float a) >= (abs_float c)) in (* Right *)
	let f2 (a,b) (c,d) = (b >= d) && ((abs_float a) >= (abs_float c)) in (* Left *)
	let f3 (a,b) (c,d) = ((abs_float b) >= (abs_float d)) && (a >= c) in (* Up *) 
	let f4 (a,b) (c,d) = ((abs_float b) >= (abs_float d)) && (a <= c) in (* Down *) 
	let f5 (a,b) (c,d) = ((abs_float (a +. b)) >= (abs_float (c +. d))) && (c >= a) in (* Up Right *)
	let f6 (a,b) (c,d) = ((abs_float (a +. b)) >= (abs_float (c +. d))) && (a >= c) in (* Down Left *)
	let f7 (a,b) (c,d) = ((abs_float (a -. b)) >= (abs_float (c -. d))) && (c >= a) in (* Down Right *)
	let f8 (a,b) (c,d) = ((abs_float (a -. b)) >= (abs_float (c -. d))) && (a >= c) in (* Up Left *) 
	[|maximal f1; maximal f2; maximal f3; maximal f4; maximal f5; maximal f6; maximal f7; maximal f8|]

(*let best_track tab s = 
	let imin = ref 0 in 
	let dmax = ref 0 in 
	let tmin = ref 10000000000 in 
	let trouve = ref false in 
	for i = 0 to (data.inters-1) do 
		if (fst !glob_graph.(s).(i)) <> -1 && tab.(i) = 0 && 
		   ((fst !glob_graph.(s).(i)) < !tmin || ((fst !glob_graph.(s).(i)) = !tmin && (snd !glob_graph.(s).(i)) >= !dmax)) then
		begin
			trouve := true;
			tmin := (fst !glob_graph.(s).(i));
			dmax := (snd !glob_graph.(s).(i));
			imin := i
		end
	done;
	(!trouve, !imin)*)

let exists_point marqs =
	let found = ref false in 
	let i = ref 0 in  
	while not !found && !i < data.inters do  
		if marqs.(!i) = 0 then
			found := true;
		incr i;
	done;
	!found

let best_track dist marqs = 
	let min_i = ref 0 in 
	let min_dist = ref 100000000. in 
	for i = 0 to data.inters - 1 do 
		if dist.(i) < !min_dist && marqs.(i) = 0 then begin
			min_i := i;
			min_dist := dist.(i)
		end
	done;
	!min_i

let returnPath prev target = 
	let l = ref [] in
	let s = ref target in 
	while prev.(!s) <> -1 do 
		l := !s :: !l;
		s := prev.(!s)
	done;
	data.depart :: !l

let returnPath2 start prev target = 
	let l = ref [] in
	let s = ref target in 
	while prev.(!s) <> -1 do 
		l := !s :: !l;
		s := prev.(!s)
	done;
	start :: !l

let rec deplace_list l =
	match l with
	|[] -> ()
	|[t] -> ()
	|t1::t2::q -> deplacer t1 t2 ; deplace_list (t2::q)

let dijkstra target = 
	let marqs = Array.make (data.inters) 0 in 
	let prev = Array.make (data.inters) (-1) in 
	let dist = Array.make (data.inters) 10000000. in 
	let continue = ref true in 
	dist.(data.depart) <- 0.;
	while !continue && exists_point marqs do 
		let best_point = best_track dist marqs in 
		marqs.(best_point) <- 1;
		if best_point = target then
			continue := false
		else begin 
			if dist.(best_point) >= 10000000. then 
				failwith "dijkstra problem";
			let t = l_accessibles best_point in 
			let n = Array.length t in 
			for i = 0 to (n-1) do
				let alt = if (snd (!glob_graph.(best_point).(t.(i))) <= 0) then dist.(best_point) +. (float_of_int (fst !glob_graph.(best_point).(t.(i))))
				else (dist.(best_point) +. ((float_of_int (fst !glob_graph.(best_point).(t.(i)))) /. (float_of_int (snd (!glob_graph.(best_point).(t.(i))))))) in
				if alt < dist.(t.(i)) then begin
					dist.(t.(i)) <- alt;
					prev.(t.(i)) <- best_point
				end
			done
		end
	done;
	returnPath prev target

let dijkstra2 start target = 
	let marqs = Array.make (data.inters) 0 in 
	let prev = Array.make (data.inters) (-1) in 
	let dist = Array.make (data.inters) 10000000. in 
	let continue = ref true in 
	dist.(start) <- 0.;
	while !continue && exists_point marqs do 
		let best_point = best_track dist marqs in 
		marqs.(best_point) <- 1;
		if best_point = target then
			continue := false
		else begin 
			if dist.(best_point) >= 10000000. then 
				failwith "dijkstra problem";
			let t = l_accessibles best_point in 
			let n = Array.length t in 
			if n = 0 then
				failwith "dijkstra problem 2";
			for i = 0 to (n-1) do
				let alt = if (snd (!glob_graph.(best_point).(t.(i))) <= 0) then dist.(best_point) +. (float_of_int (fst !glob_graph.(best_point).(t.(i))))
								else (dist.(best_point) +. ((float_of_int (fst !glob_graph.(best_point).(t.(i)))) /. (float_of_int (snd (!glob_graph.(best_point).(t.(i))))))) in
				if alt < dist.(t.(i)) then begin
					dist.(t.(i)) <- alt;
					prev.(t.(i)) <- best_point
				end
			done
		end
	done;
	returnPath2 start prev target


let main () = 
	let t = etoile () in
	let tl = Array.make 8 [] in
	let timeleft = Array.make 8 data.temps in
	(* Les 8 au periph *)
	for i = 0 to 7 do 
		glob_timeleft := data.temps;
		let l' = dijkstra t.(i) in 
		(* movelist l'; *)
		tl.(i) <- l' ;
		Printf.printf "Voiture %i periph\n" i;
		timeleft.(i) <- !glob_timeleft
	done;
	(* Les 8 en parallèe *)
	let voitures_restantes = ref 8 in
	let still_running = Array.make 8 true in
	let compteur = ref 100 in
	while !compteur > 0 && !voitures_restantes > 0 do
		for i = 0 to 7 do
			if still_running.(i) then begin
				try begin
					let pos = List.hd (List.rev tl.(i)) in
					(* Printf.printf "pos = %i\n" pos; *)
					let l = dijkstra2 pos (List.hd (List.rev tl.((i+2) mod 8))) in
					(* Printf.printf "%i\n" (List.length l); *)
					let a::b::c::_ = l in
					(* Printf.printf "%i %i %i\n" a b c; *)
					glob_timeleft := timeleft.(i);
					deplace_list (a::b::c::[]);
					timeleft.(i) <- !glob_timeleft;
					tl.(i) <- (tl.(i))@[b;c];
					(* Printf.printf "Ok" *)
				end
				with _ -> begin
					still_running.(i) <- false;
					decr voitures_restantes
				end
			end
		done;
		decr compteur
	done;
	Printf.printf "SCORE : %i\n" !score;
	output [tl.(0) ; tl.(1) ; tl.(2) ; tl.(3) ; tl.(4) ; tl.(5) ; tl.(6) ; tl.(7)] "result.txt"

let () = 
	Random.self_init ();
	input "paris_54000.txt";
	center_gps ();
	main ()