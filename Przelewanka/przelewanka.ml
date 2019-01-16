(* Zadanie: Przelewanka *)
(* Autor: Michał Niedziółka *)
(* Code review: Kajetan Husiatyński *)

let przelewanka tab = 
	let rec gcd a b =
        if a = 0 then b
        else gcd (b mod a) a
    in let gcd_all = Array.fold_left (fun a (x, _) -> gcd (min x a) (max x a)) 0 tab in

    (* Rozwiązanie nie istnieje jeśli nie ma ani jednej pustej/pełnej szklanki *)
    (* Rozwiązanie nie istnieje jeśli istnieje szklanka w której końcowa ilość wody nie jest podzielna przez NWD pojemności *)
	let exists tab = 
		let check = ref 0 in
		begin
		for i = 0 to ((Array.length tab) - 1) do
			if snd tab.(i) == 0 || fst tab.(i) == snd tab.(i) then check := 1;
		done;
		
		for i = 0 to ((Array.length tab) - 1) do
			if snd tab.(i) mod gcd_all <> 0 then check := 0;
		done;

		!check
		end

	and solve tab = 
		let pour k state = 
		let listates = ref [] in
		begin
			let curr = Array.copy state in
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
		end;	!listates

		(* Trzymam hashe wszystkich stanów które kiedykolwiek odwiedziłem żeby zapobiec niepotrzebnym odwiedzeniom *)
		(* Zaczynam od stanu zero czyli wszystkie szklanki puste *)
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
			(* Chodzę BFSem dopóki nie przejdę wszystkich możliwych stanów lub nie znajdę rozwiązania *)
			Hashtbl.add hash_table hash (-1);
			if Hashtbl.mem hash_table state then begin flag := 1; end
			else Hashtbl.add hash_table state 0;
			Queue.add state queue;
			while (not (Queue.is_empty queue)) && !flag = 0 do
				let curr = Queue.take queue in
				let dist = Hashtbl.find hash_table curr in
				begin
					for i = 0 to ((Array.length curr) - 1) do
						let re = Array.copy curr and sp = Array.copy curr and po = pour i curr in
						begin
							re.(i) <- fst tab.(i);
							sp.(i) <- 0;

							List.iter (fun el -> if (not (Hashtbl.mem hash_table el)) then begin Hashtbl.add hash_table el (dist+1); Queue.add el queue; end
																	   else if Hashtbl.find hash_table el = (-1) then 
																	   begin flag := 1; result := (dist + 1); end 
																	   else ()) (re::(sp::po));
						end
					done 
				end
			done;
			if !flag = 0 then (-1) else !result
		end


	in if gcd_all = 0 then 0 else if exists tab = 1 then solve tab else if (Array.length tab) = 0 then 0 else (-1)
