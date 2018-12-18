(* Filename:   tj983-hw3.sml
Assignment 3:    ML 
Author:    Tulsi Jain *)

(* Question 2  Starts here *)

(* 2.1 Given a list of ’a option, outputs ’a list such None type are removed *)
fun filteropt [] = [] 
| filteropt (x ::y) = case x of NONE => filteropt(y) 
						| SOME k => k::filteropt(y);

(* 2.2 Bind two option datatype to a function if both of type SOME otherwise None *)
fun bind a b func12 = case a of NONE => NONE 
						| SOME y => case b of NONE => NONE 
						| SOME z => SOME (func12 y z);

(* 2.3 Given an integer n and a list, evaluates to the nth item in the list *)
fun getitem n list = 
	if null list then NONE 
	else 
		if n=1 then (SOME (hd list)) 
		else getitem (n - 1) (tl list);

(* 2.4 Given an Option int n and a list, evaluates to the nth item in the list if n is not None *)
fun getitem2 n list = case n of NONE => NONE 
						| SOME y => getitem y list;

(* helper function for 2.5 *)
fun helper list1 item y = 
	if null list1 then NONE 
	else 
		if y=1 then SOME(((hd list1) item)) 
		else (helper (tl list1) item (y - 1));

(* 2.5 Select a function based of output of another function *)
fun selectf func1 list1 item = case (func1 item) of NONE => NONE | SOME y => (helper list1 item y); 

(* helper function for 2.6 *)
fun helper6 functlist whichfunction argumentlist = if null whichfunction then [] else case (hd whichfunction) of NONE => NONE::(helper6 functlist (tl whichfunction) (tl argumentlist)) 
| SOME y => (helper functlist (hd argumentlist) y)::(helper6 functlist (tl whichfunction) (tl argumentlist));

(* 2.6 Select a function based of output of another function *)
fun funcmap func functlist argumentlist =  ( filteropt (helper6 functlist (map func argumentlist) argumentlist));



(* Question 3 Starts here *)
(* Signature definition for type and maximum level of a multi-level queue *)
signature MLQPARAM = sig
	type element;
	val max : int;
end;

(* MakeQ functor definition *)
functor MakeQ (MP:MLQPARAM):
sig
	(* Type declaration *)
	type 'label mlqueue

	(* Exception declaration *)
	exception Overflow
	exception Empty
	exception LevelNoExist
	exception NotFound

	(* Maximum level type *)
	val maxlevel : int

	(* New Queue Type *)
	val new: MP.element mlqueue

	(* Enqueue a element in a mlqueue declaration *)
	val enqueue : MP.element mlqueue -> int -> int -> MP.element -> MP.element mlqueue

	(* Dequeue a element from a mlqueue declaration *)
	val dequeue : MP.element mlqueue -> MP.element * MP.element mlqueue

	(* Move elements in a queue satisfies the given predicate declaration *)
	val move : (MP.element -> bool) -> MP.element mlqueue -> MP.element mlqueue

	(* A list of element at a given level declaration*)
	val atlevel : MP.element mlqueue -> int -> (int * MP.element) list

	(* First element that satisfy the given predicate declaration *)
	val lookup : (MP.element -> bool) -> MP.element mlqueue -> int * int

	(* Check if queue is empty declaration *)
	val isempty : MP.element mlqueue -> bool

	(* A list of element in queue declaration *)
	val flatten : MP.element mlqueue -> MP.element list
end = struct
	
	open MP;

	type 'label mlqueue = (int*int*MP.element) list;

	(* Overflow exception *)
	exception Overflow;

	(* Empty exception *)
	exception Empty;

	(* LevelNoExist exception *)
	exception LevelNoExist;

	(* NotFound exception *)
	exception NotFound;

	val maxlevel  = MP.max;

	(* Returns a empty queue *)
	val new : MP.element mlqueue = [];

	(* Dequeue a element from a  queue otherwise raise an exeption Empty *)
	fun dequeue ([] : MP.element mlqueue) = raise Empty 
	| dequeue (((x,y,z):: (xs : MP.element mlqueue) ): MP.element mlqueue) = (z , xs);

	(* Enqueue a element in a queue, if inserted level is greater than maxlevel then raise LevelNoExist exception *)
	fun enqueue ( [] : MP.element mlqueue) level prior element = 
		if level > maxlevel then raise LevelNoExist 
		else (((level , prior , element)::[]) :MP.element mlqueue)
	| enqueue (((x,y,z)::xs): MP.element mlqueue) level prior element = 
		if level > maxlevel then raise LevelNoExist 
		else 
			if x > level then ((level, prior, element)::(x, y, z)::xs : MP.element mlqueue) 
			else 
				if x = level then 
					if y > prior then ((level, prior, element)::(x, y, z)::xs : MP.element mlqueue) 
					else 
						if y = prior then ((x, y, z)::(level, prior, element)::xs : MP.element mlqueue) 
						else ((x, y, z)::(level, prior, element)::xs : MP.element mlqueue) 
				else ((x, y, z)::(enqueue xs level prior element) : MP.element mlqueue);
	
	(* Move a element to next level if exists and satisfy a given predicate *)
	fun move pred ([] : MP.element mlqueue) = []
	| move pred (((x,y,z)::xs): MP.element mlqueue) = 
		if pred z andalso x < maxlevel then enqueue (move pred (xs : MP.element mlqueue)) (x+1) y z 
		else enqueue (move pred (xs : MP.element mlqueue)) x y z;

	(* Returns a list of element with prioritty at a given level, if level does not exist then raise LevelNoExist exception *)
	fun atlevel ([] : MP.element mlqueue) n  = 
		if n > maxlevel then raise LevelNoExist 
		else []
	| atlevel (((x,y,z)::xs): MP.element mlqueue) n  = 
		if n > maxlevel then raise LevelNoExist 
		else 
			if n < x then [] 
			else 
				if n=x then (y,z)::atlevel xs n 
				else atlevel (xs : MP.element mlqueue) n;

	(* Returns a first element that satisfy the given predicate *)
	fun lookup pred ([] : MP.element mlqueue) = raise NotFound 
	| lookup pred (((x,y,z)::xs): MP.element mlqueue) = 
		if pred z then (x,y) 
		else (lookup pred xs);

	(* Returns true if list is empty else return false *)
	fun isempty (lst : MP.element mlqueue) = 
		if null lst then true 
		else false;

	(* Returns a flatten list of multi level queue *)
	fun flatten ([] : MP.element mlqueue) = [] 
	| flatten (((x,y,z)::xs): MP.element mlqueue)= z::(flatten xs);
end;

(* Declaration of element type and maximum level *)
structure Integer : MLQPARAM =
struct
	type element = int ;
	val max = 2;
end;

(* 3.1 Creating a functior, IntegerMLQ object *)
structure IntegerMLQ = MakeQ(Integer);

(* 3.2 Creating a empty queue*)
val k = IntegerMLQ.new;
(* Enqueuing six elements in a queue*)
val f1 = IntegerMLQ.enqueue k 1 1 2;
val f2 = IntegerMLQ.enqueue f1 0 0 3;
val f3 = IntegerMLQ.enqueue f2 2 0 5;
val f4 = IntegerMLQ.enqueue f3 2 2 1;
val f5 = IntegerMLQ.enqueue f4 1 0 4;
val f6 = IntegerMLQ.enqueue f5 2 1 6;

(* 3.3 Raise a exception because inserted element exceeds maximum level*)
(*val f7 = IntegerMLQ.enqueue f6 3 1 6;*)

(* 3.4 Moves elements satisfy the pred1*)
fun pred1 a = if a > 3 then true else false;
val f7 = IntegerMLQ.move pred1 f6;

(* 3.5 Dequeue two elements *)
val (numebr1, f8) = IntegerMLQ.dequeue f7;
val (numebr2, f9) = IntegerMLQ.dequeue f8;

(* 3.6 Retuen a list of element at level 1 *)
val f10 = IntegerMLQ.atlevel f9 1;

(* 3.7 Retuen a first element satisfy the pred2 *)
fun pred2 a = if a <5  then true else false;
val f11 = IntegerMLQ.lookup pred2 f9;