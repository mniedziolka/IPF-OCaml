(* Zadanie: Przelewanka *)
(* Autor: Michał Niedziółka *)
(* Code review: Kajetan Husiatyński *)

let przelewanka tab = 
	let exists tab = 
		let check = ref 0 in
		begin
		for i = 0 to ((Array.length tab) - 1) do
			if fst tab.(i) == 0 || fst tab.(i) == snd tab.(i) then check := 1;
		done;
		!check
		end

	and solve tab = 
		let refill k state = begin state.(k) <- fst tab.(k); state; end
		and spill k state = begin state.(k) <- 0; state; end
		and pour k state = 
		begin
			let listates = ref []
			and curr = Array.copy state in
			for i = 0 to ((Array.length state) - 1)do
				let state = Array.copy curr in
				let size = (fst tab.(i)) - curr.(i) in
				if size > state.(k) then
					begin
						state.(i) <- state.(i) + state.(k);
						state.(k) <- 0;
						listates := state::!listates;
					end
				else
					begin
						state.(i) <- fst tab.(i);
						state.(k) <- state.(k) - size;
						listates := state::!listates;
					end
			done;	
			!listates
		end

		in let hash_table = Hashtbl.create 1488997 
		and queue = Queue.create () 
		and state = Array.make (Array.length tab) 0 
		and hash = Array.make (Array.length tab) 0
		and result = ref 0
		and flag = ref 0 in  
		begin
			for i = 0 to ((Array.length tab) - 1) do
				hash.(i) <- snd tab.(i);
			done;
			Hashtbl.add hash_table hash (-1);
			Hashtbl.add hash_table state 0;
			Queue.add state queue;
			while (not (Queue.is_empty queue)) && !flag = 0 do
				let curr = Queue.take queue in
				let dist = Hashtbl.find hash_table curr in
				begin
					print_int 1;
					for i = 0 to ((Array.length curr) - 1) do
						let re = refill i curr and sp = spill i curr and po = pour i curr in
						begin
							List.iter (fun el -> if (not (Hashtbl.mem hash_table el)) then begin Hashtbl.add hash_table el (dist+1); Queue.add el queue; end
																	   else if Hashtbl.find hash_table el = (-1) then 
																	   begin flag := 1; result := (dist + 1); end 
																	   else ()) (re::(sp::po))
						end
					done 
				end
			done;
			!result
		end


	in if exists tab = 1 then solve tab else (-1)
