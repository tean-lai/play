module Read = struct
  let read_int () = read_int ()

  let read_int_list () =
    read_line () |> String.split_on_char ' ' |> List.map int_of_string

  let read_int_pair () = Scanf.scanf "%d %d\n" (fun a b -> (a, b))

  let read_int_pairs n =
    let rec aux acc i =
      if i = n then List.rev acc
      else
        let pair = read_int_pair () in
        aux (pair :: acc) (i + 1)
    in
    aux [] 0

  let read_int_grid rows cols =
    let rec read_row acc_row row_num =
      if row_num = rows then List.rev acc_row
      else
        let row = read_int_list () in
        read_row (row :: acc_row) (row_num + 1)
    in
    read_row [] 0

  let read_words () =
    let line = read_line () in
    String.split_on_char ' ' line

  let read_float_list () =
    read_line () |> String.split_on_char ' ' |> List.map float_of_string

  let read_lines n =
    let rec aux acc i =
      if i = n then List.rev acc
      else
        let line = read_line () in
        aux (line :: acc) (i + 1)
    in
    aux [] 0

  let read_char_matrix rows =
    let rec read_row acc_row row_num =
      if row_num = rows then List.rev acc_row
      else
        let row = read_line () |> String.to_seq |> List.of_seq in
        read_row (row :: acc_row) (row_num + 1)
    in
    read_row [] 0
end

module Algebra = struct
  module type RingDef = sig
    type t

    val zero : t
    val one : t
    val ( + ) : t -> t -> t
    val ( * ) : t -> t -> t
    val ( ~- ) : t -> t
    val to_string : t -> string
  end

  module IntRingDef = struct
    type t = int

    let zero = 0
    let one = 1
    let ( + ) = ( + )
    let ( * ) = ( * )
    let ( ~- ) = ( ~- )
    let to_string = string_of_int
  end

  module MakeRing (RingDef : RingDef) = struct
    type t = RingDef.t

    let zero = RingDef.zero
    let one = RingDef.one
    let ( + ) = RingDef.( + )
    let ( * ) = RingDef.( * )
    let ( ~- ) = RingDef.( ~- )
    let to_string = RingDef.to_string

    let rec pow x n =
      if n = 0 then one
      else
        let halfpow = pow x (n / 2) in
        if n mod 2 = 0 then halfpow * halfpow else x * halfpow * halfpow
  end

  module TwoByTwoMatrixRingDef = struct
    type t = {
      a : int;
      b : int;
      c : int;
      d : int;
    }

    let make a b c d : t = { a; b; c; d }
    let zero = make 0 0 0 0
    let one = make 1 0 0 1
    let ( + ) a b = make (a.a + b.a) (a.b + b.b) (a.c + b.c) (a.d + b.d)

    let ( * ) x y =
      let ( + ) = Int.add in
      make
        ((x.a * y.a) + (x.b * y.c))
        ((x.a * y.b) + (x.b * y.d))
        ((x.c * y.a) + (x.d * y.c))
        ((x.c * y.b) + (x.d * y.d))

    let ( ~- ) x = make (-x.a) (-x.b) (-x.c) (-x.d)
    let to_string x = failwith "todo"
  end

  module IntRing = MakeRing (IntRingDef)
  module TwoByTwoMatrixRing = MakeRing (TwoByTwoMatrixRingDef)

  let intpow (a : int) (b : int) : int = IntRing.pow a b

  let rec fib_slow n =
    if n = 0 || n = 1 then n else fib_slow (n - 1) + fib_slow (n - 2)

  let fib_fast n =
    let base = TwoByTwoMatrixRingDef.make 1 1 0 1 in
    let powed = TwoByTwoMatrixRing.pow base n in
    powed.b

  let fib = fib_slow
  let rec gcd a b = if b = 0 then a else gcd b (a mod b)
  let lcm a b = a * b / gcd a b
  let extended_euclidean a b = failwith "todo"
  let linear_diophantine a b = failwith "todo"

  let sieve n =
    let is_prime = Array.make (n + 1) true in
    is_prime.(0) <- false;
    is_prime.(1) <- false;
    for i = 2 to n do
      if is_prime.(i) && i * i <= n then
        let j = ref (i * i) in
        while !j <= n do
          is_prime.(!j) <- false;
          j := !j + 1
        done
    done;
    is_prime
end

module DataStructure = struct
  module SegmentTree = struct
    type t = {
      n : int;
      t : int array;
    }

    let build (a : int array) v tl tr =
      let n = Array.length a in
      let t = Array.make (4 * n) 0 in
      let rec build_helper v tl tr =
        if tl = tr then t.(v) <- a.(tl)
        else
          let tm = (tl + tr) / 2 in
          build_helper (v * 2) tl tm;
          build_helper ((v * 2) + 1) (tm + 1) tr;
          t.(v) <- t.(v * 2) + t.((v * 2) + 1)
      in
      build_helper v tl tr;
      { n; t }

    let rec sum st v tl tr l r =
      let t = st.t in
      if l > r then 0
      else if l == tl && r == tr then t.(v)
      else
        let tm = (tl + tr) / 2 in
        sum st (v * 2) tl tm l (min r tm)
        + sum st ((v * 2) + 1) (tm + 1) tr (max l (tm + 1)) r

    let rec update st v tl tr pos new_val =
      let t = st.t in
      if tl = tr then t.(v) <- new_val
      else
        let tm = (tl + tr) / 2 in
        if pos <= tm then update st (v * 2) tl tm pos new_val
        else update st ((v * 2) + 1) (tm + 1) tr pos new_val;
        t.(v) <- t.(v * 2) + t.((v * 2) + 1)
  end
end

module Algorithm = struct
  let binsearch_only lo hi f =
    ignore (assert lo <= hi);
    failwith "todo"

  let binsearch_first lo hi f =
    ignore (assert lo <= hi);
    failwith "todo"

  let binsearch_last lo hi f =
    ignore (assert lo <= hi);
    failwith "todo"
end

let tests =
  [
    (let fib = Algebra.fib in
     [ fib 0 = 0; fib 1 = 1; fib 6 = 8 ]);
    (let pow = Algebra.intpow in
     [ pow 2 2 = 4; pow 2 1 = 2; pow 3 3 = 27 ]);
  ]

let () =
  tests |> List.fold_left ( @ ) []
  |> List.map (fun b -> assert b)
  |> ignore
