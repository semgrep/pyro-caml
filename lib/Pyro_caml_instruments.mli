val emit_point_event : Printexc.raw_backtrace -> unit

val with_memprof_sampler : ?sampling_rate:float -> (unit -> 'a) -> 'a

val maybe_with_memprof_sampler : ?sampling_rate:float -> (unit -> 'a) -> 'a

val create_cursor : string -> int -> Runtime_events.cursor

val read_poll :
     ?max_events:int option
  -> ?callbacks:Runtime_events.Callbacks.t
  -> Runtime_events.cursor
  -> float
  -> Stack_trace.t list
