(* Zadanie: Przelewanka *)
(* Autor: Michał Niedziółka *)
(* Code review: Kajetan Husiatyński *)

let przelewanka tab = 
	let exists tab = 
		let check = ref 0 in
		begin
		for i = 0 to Array.length tab do
			if fst tab.(i) == 0 || fst tab.(i) == snd tab.(i) then check := 1;
		done;
		!check
		end

	and solve tab = 
		let refill k state = let s = snd state.(k) in begin state.(k) <- (s, s); state; end
		and spill k state = let s = snd state.(k) in begin state.(k) <- (0, s); state; end
		and pour k state = 
		begin
			let listates = ref [] in
			for i = 0 to Array.length tab do
				let curr = Array.copy tab in
				let size = snd curr.(i) - fst curr.(i) in
				if size >  fst curr.(k) then
					begin
					curr.(i) <- (fst curr.(i) + fst curr.(k), snd curr.(i));
					curr.(k) <- (0, snd curr.(k));
					curr
					end
				else
					begin
					curr.(i) <- (snd curr.(i), snd curr.(i));
					curr.(k) <- (fst curr.(k) - size, snd curr.(k));
					curr
					end
			done
		end

		in let hash_table = Hashtbl.create 1488997 
		and queue = Queue.create () 
		and state = Array.make (Array.length tab) 0 
		and flag = ref 0 in  
		begin
			Hashtbl.add hash_table tab (-1);
			Hashtbl.add hash_table state 0;
			Queue.add queue state;
			while (not Queue.is_empty queue) &&	 flag = 0 do
				let curr = Queue.take queue in
				let dist = Hashtbl.find hash_table state in
				begin
					for i = 0 to (Array.length state) do
						let re = refill i state and sp = spill i state and po = pour i state in
						begin
							List.iter (fun el -> if not Hashtbl.mem el then begin Hashtbl.add hash_table el dist+1; Queue.add queue el; end
																	   else if Hashtbl.find hash_table el == -1 then begin flag := 1; result := dist + 1; end 
																	   else ()) re::(sp::po)
						end
					done 
				end
			done
		end


	in if exists tab = 1 then solve tab else -1
