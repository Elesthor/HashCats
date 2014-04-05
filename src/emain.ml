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


let () = 
  input "paris_54000.txt";
  let m = !glob_graph in 
  let inter= data.inters in
  let r = data.rues in
  let t = data.temps in
  let f = data.flotte in
  let d = data.depart in
  Printf.printf "coucou\n";
  let rec road (visite:(int*int) list) c time : (int*int) list =
	if (c>2000) then
	  begin
		Printf.printf "time : %i, %i\n" time t;
		visite
	  end
	else
	  begin
		let (i,j)= List.hd(visite) in
		let max = ref 0 in
		for x=0 to inter-1 do
		  if not(List.mem (j, x) visite) && x<>i && x<>j then
			if  m.(j).(x).long > (!max) then
			  if time+m.(j).(x).cout+time<=t then
				max:= x;
		  
		done;
		Printf.printf "%i\n" !max;
		if !max <> 0 then
		  road ((j,!max)::visite) (time+m.(j).(!max).cout) (c+1) 
		else
		  visite
	  end
  in
  List.iter (fun (x,y) -> Printf.printf "(%i,%i)\n" x y ) (road [(d,d)] 0 0)
