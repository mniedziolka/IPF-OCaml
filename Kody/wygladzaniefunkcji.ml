let wygladzenie dx f = fun x -> (f(x - dx) + f(x) + f(x + dx)) /. 3;;