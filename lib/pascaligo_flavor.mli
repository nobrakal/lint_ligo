type flavor = Terse | Verbose

val string_of_flavor : flavor -> string

type wrong_flavor =
  { actual   : flavor;
    expected : string;
    got      : string;
    reg      : Simple_utils.Region.region
  }

(* Verify if the CST is coherent with respect to the flavor.
   If not specified, the falvor is inferred. *)
val run : ?flavor:flavor -> Pascaligo.CST.t -> wrong_flavor option

val format : wrong_flavor option -> (Simple_utils.Location.t * string) list
