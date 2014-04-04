open Printf;; 

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

let test_rect x y t m =
  let r = ref 1 in
  for i = -t to t do
    for j = -t to t do 
      if m.(x+i).(y+j) == 0 then
        r :=  0
    done;
  done; 
  !r

let rm_rect x y t m =
  for i = -t to t do
    for j = -t to t do 
        m.(x+i).(y+j) <-  0
    done;
  done 



let print_r i j m out = 
  begin
  Printf.fprintf out "PAINTSQ %i %i %i\n" i j m
  end



let () = 
  let x = input "doodle.txt" in
  let y = open_out "test" in 
  Printf.fprintf y  "PAINTSQ %i %i %i\n" 0 0 0;
  for e = 100  downto 0 do 
    print_int e;print_newline();
  for i = 0 to (Array.length x) -1 do
    for j = 0 to (Array.length x.(0))-1 do
      if x.(i).(j) == 1  then 
        begin
        try
          if 1 == test_rect i j e x then
            begin 
              print_r i j e y;
              rm_rect i j e x
            end
           with
            _ -> ()
        end
      done
  done
  done
