type 'a t

val create :
  int ->
  ?check:('a -> (bool -> unit) -> unit) ->
  ?validate:('a -> bool Lwt.t) ->
  (unit -> 'a Lwt.t) ->
  'a t

val use : 'a t -> ('a -> 'b Lwt.t) -> 'b Lwt.t
