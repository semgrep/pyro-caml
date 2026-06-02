val with_memprof_sampler : ?sampling_rate:float -> (unit -> 'a) -> 'a
(** [with_memprof_sampler ?sampling_rate f] enables the Pyro Caml sampler for
    the duration of [f]. An optional [sampling_rate] can be passed which will be
    given to the underlying {!Gc.Memprof} profiler. By default it is [1e-6], but
    choose a higher value such as [1e-4] if there the profiler sample rate is
    high, or not many allocations happen in your program. Alternatively if not
    many allocations happen, use {!emit_point_event} or the Pyro Caml PPX *)

val maybe_with_memprof_sampler : ?sampling_rate:float -> (unit -> 'a) -> 'a
(** [maybe_with_memprof_sampler ?sampling_rate f] is similar to
    {!with_memprof_sampler} except it only enables the sampler if the env var
    [OCAML_RUNTIME_EVENTS_START] is set*)

val emit_point_event : Printexc.raw_backtrace -> unit
(** [emit_point_event (Printexc.get_callstack max_int)] will record a stack
    trace to the profiler. This is useful if you are in code that might not
    allocate much and you want to ensure you're still generating enough sample
    points. *)

val create_cursor : string -> int -> Runtime_events.cursor
(** [create_cursor path pid] creates a cursor for reading runtime events
    from the given [path] and [pid]. *)

val read_poll :
  ?max_events:int option -> Runtime_events.cursor -> float -> float -> Stack_trace.t list
(** [read_poll cursor sample_interval max_delta] will read the profiling runtime events
    from the given cursor, and will give attempt to give a single
    {!Stack_trace.t} per every unique thread id, within [sample_interval] of the
    start time of this function call and within [max_delta] of the sample time.
    If a sample is not within these intervals, they will not be considered.
    Additionally it also ensures the samples chosen are those closest to the
    start of the call of this function. *)
