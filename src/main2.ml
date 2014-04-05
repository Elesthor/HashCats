type mapdata = {mutable inters : int; mutable rues : int; mutable temps : int; mutable flotte : int; mutable depart : int}
type streetdata = {mutable cout : int; mutable long : int}

let glob_graph = ref (Obj.magic ())
let glob_gps = ref (Obj.magic ())
let data = {inters = 0; rues = 0; temps = 0; flotte = 0; depart = 0}

let score = ref 0
let score_prec = ref (-1)
let solutions = ref 0
let glob_timeleft = ref 0

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

let distance lat1 lon1 lat2 lon2 : float =
	let r = 6371. in
	let dlat = deg2rad (lat2 -. lat1) in
	let dlon = deg2rad (lon2 -. lon1) in
	let a : float =
		(sin (dlat/.2.)) *. (sin (dlat/.2.)) +.
		(cos (deg2rad lat1)) *. (cos (deg2rad lat2)) *.
		(sin (dlon/.2.)) *. (sin (dlon /. 2.))
	in
	let c = 2. *. atan((sqrt a) /. (sqrt (1.-.a))) in
	r *. (abs_float c)

let distance2 (a,b) (c,d) = 
	distance a b c d

let plus_proche i j =
	let (lat2, lon2) = (!glob_gps).(j) in
	let t = l_accessibles i in 
	let n = Array.length t in
	if n = 0 then
		failwith "No more time"
	else begin 
		let retour = ref 0 in
		let d = ref 100000000. in
		for x = 0 to n - 1 do
			let (lat3,lon3) = (!glob_gps).(t.(x)) in
			let d' = distance lat2 lon2 lat3 lon3 in
			if d' < !d then begin
				retour := t.(x);
				d := d'
			end
		done;
		!retour
	end

let itineraire i j =
	let l = ref [i] in
	let intermediaire = ref (plus_proche i j) in
	deplacer i (!intermediaire);
	l := !intermediaire :: (!l);
	let ite = ref 10 in
	while !ite >= 0 do
		let inter_prec = !intermediaire in 
		intermediaire := plus_proche !intermediaire j;
		deplacer inter_prec (!intermediaire);
		l := !intermediaire :: (!l);
		print_int !intermediaire;
		print_endline "";
		decr ite
	done;
	List.rev (!l)

let main () = 
	let t = etoile () in
	let l = ref [] in 
	for i = 0 to 7 do 
		glob_timeleft := data.temps;
		l := (itineraire data.depart t.(i)) :: !l 
	done;
	Printf.printf "SCORE : %i\n" !score;
	output !l "result.txt"

let () = 
	Random.self_init ();
	input "paris_54000.txt";
	center_gps ();
	main ()