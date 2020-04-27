class type ['a] offer = object
  method accept : Switch.t -> 'a Lwt.t
  method decline : unit
end

class type ['a] t = object
  method get : ('a offer, [`Busy of string * unit Lwt.t]) result Lwt.t
  method pp : Format.formatter -> unit
end

val create : label:string -> int -> unit t

val pair : 'a t -> 'b t -> ('a * 'b) t
(** [pair a b] is a virtual pool that takes one item from [a] and one from [b]. *)

val both : unit t -> unit t -> unit t
(** [both a b] is like [pair] but where there is no result. *)

val either : 'a t -> 'a t -> 'a t
(** [either a b] is a virtual pool that takes one item from either [a] or [b]. *)

val pp : _ t Fmt.t
