(* Subtask 1: Binomial Coefficient *)
fun binomial n 0 = 1
  | binomial 0 k = 0
  | binomial n k = (binomial (n - 1) (k - 1) * n) div k

(* Subtask 2: Pascal's Triangle and Binomial Coefficient using Pascal's Triangle *)
fun nextRow row = 
    let
        val row' = 0 :: row
        val row'' = row @ [0]
    in
        ListPair.map op+ (row', row'')
    end

fun pascal () =
    let
        fun rows [] = [[1]]
          | rows (r::rs) = let val next = nextRow r in r :: rows (next::rs) end
    in
        rows [[1]]
    end

fun binomial2 n k =
    let
        fun factorial 0 = IntInf.fromInt 1
          | factorial m = IntInf.* (factorial (m - 1), IntInf.fromInt m)

        fun binomialCoeff n k = 
            let
                val numerator = factorial n
                val denominator = IntInf.* (factorial k, factorial (n - k))
            in
                IntInf.div (numerator, denominator)
            end
    in
        binomialCoeff n k
    end


(* Subtask 3: Merge Sort *)
fun merge (xs, []) = xs
  | merge ([], ys) = ys
  | merge (x::xs, y::ys) = if x <= y then x :: merge (xs, y::ys) else y :: merge (x::xs, ys)

fun mergesort [] = []
  | mergesort [x] = [x]
  | mergesort xs = 
    let
        val n = List.length xs div 2
        val ys = List.take(xs, n)
        val zs = List.drop(xs, n)
    in
        merge (mergesort ys, mergesort zs)
    end

(* Subtask 4: Extended Euclidean Algorithm *)
exception NoSolution

fun extendedGCD 0 b = (b, 0, 1)
  | extendedGCD a b =
    let
        val (g, x, y) = extendedGCD (b mod a) a
    in
        (g, y - (b div a) * x, x)
    end

fun de a b z = 
    let
        val (g, x, y) = extendedGCD a b
    in
        if z = g then (x, y, z) else raise NoSolution
    end

(* Subtask 5: Prime Factors *)
fun primeFactors n = 
    let
        fun factor (n, p) =
            if p * p > n then if n > 1 then [n] else []
            else if n mod p = 0 then p :: factor (n div p, p)
            else factor (n, p + 1)
    in
        factor (n, 2)
    end

(* Subtask 6: Euler's Totient Function *)
fun gcd (x, y) = 
    let
        fun gcd' 0 b = b
          | gcd' a b = gcd' (b mod a) a
    in
        gcd' x y
    end

fun totient n = 
    List.length (List.filter (fn x => gcd (x, n) = 1) (List.tabulate (n, fn i => i + 1)))

fun totient n = 
    List.length (List.filter (fn x => gcd (x, n) = 1) (List.tabulate (n, fn i => i + 1)))

(* Subtask 7: Euler's Totient Function Using Prime Factors *)
fun intPow (x, 0) = 1
  | intPow (x, n) = x * intPow (x, n - 1)

fun group [] = []
  | group (x::xs) = 
    let
        val (prefix, suffix) = List.partition (fn y => y = x) xs
    in
        (x::prefix) :: group suffix
    end

fun totient2 n =
    let
        fun product (p, k) = (p - 1) * intPow (p, k - 1)
        val factors = primeFactors n
        val groupedFactors = group factors
        val factorCounts = List.map (fn xs => (List.hd xs, List.length xs)) groupedFactors
    in
        List.foldl (fn (pk, acc) => acc * product pk) 1 factorCounts
    end


(* Subtask 8: Prime Numbers Up to a Given Number *)
fun isPrime p = 
    let
        val sqrtP = Real.floor (Math.sqrt (Real.fromInt p))
        fun checkDivisors d = d > sqrtP orelse (p mod d <> 0 andalso checkDivisors (d + 1))
    in
        checkDivisors 2
    end

fun primes n = 
    List.filter isPrime (List.tabulate (n - 1, fn i => i + 2))


(* Main Function *)
fun listToString xs = "[" ^ String.concatWith ", " (List.map Int.toString xs) ^ "]"

fun main () =
    let
        val _ = print (Int.toString (binomial 200 5) ^ "\n")
        val _ = print (IntInf.toString (binomial2 200 5) ^ "\n")
        val _ = print (listToString (mergesort [4, 3, 2, 1]) ^ "\n")
        val _ = print (case (de 35 15 5) of (x, y, z) => "(" ^ Int.toString x ^ ", " ^ Int.toString y ^ ", " ^ Int.toString z ^ ")\n")
        val _ = print (listToString (primeFactors 100) ^ "\n")
        val _ = print (Int.toString (totient 100) ^ "\n")
        val _ = print (Int.toString (totient2 100) ^ "\n")
        val _ = print (listToString (primes 100))
    in
        ()
    end

val _ = main ()
