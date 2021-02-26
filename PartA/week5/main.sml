(* Mutual recursion *)

(* Modules for Namespace Management *)
structure MyModule = struct
  fun add_one(x) =
    x + 1
end

val res = MyModule.add_one(1)
(* open MyModule: put stuff from MyModule in global env*)

(* Signatures and Hiding Things *)
signature Math =
sig
  val add_one : int -> int
end


structure MyModuleLib :> Math =
struct
  fun add_one(x) =
    x + 1
end
