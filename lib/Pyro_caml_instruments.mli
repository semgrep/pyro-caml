val with_memprof_sampler : (unit -> 'a) -> 'a
(** [with_memprof_sampler f] enables the Pyro Caml sampler for the duration
    of [f]. [memprof_rate] (formerly sampling_rate) is now passed from the
    rust side via the env var [OCAML_MEMPROF_SAMPLING_RATE]. This is because
    memory allocation tracking relies on [memprof_rate] to put out accurate
    measurements. In particular, the profiler scales recorded allocation counts
    by [1/memprof_rate]. Throws an error if [OCAML_MEMPROF_SAMPLING_RATE] is
    not set or is not a valid float. *)

val maybe_with_memprof_sampler : (unit -> 'a) -> 'a
(** [maybe_with_memprof_sampler f] is similar to {!with_memprof_sampler} except
    it only enables the sampler if the env var [OCAML_RUNTIME_EVENTS_START] is
    set. Also throws an error if [OCAML_MEMPROF_SAMPLING_RATE] is not set or is
    not a valid float. *)

val emit_point_event :
  Printexc.raw_backtrace -> n_samples:int -> size:int -> Event.point
(** [emit_point_event (Printexc.get_callstack max_int) ~n_samples:0 ~size:0]
    records a single sample point (a stack trace plus its allocation counts) to
    the profiler.

    [n_samples] is the number of memprof samples attributed to this allocation;
    the profiler scales it by [1/sampling_rate] to estimate the total amount of
    allocations. [size] is the size of the allocated block in words, excluding
    the header. Used in estimating the number of objects. 

    Passing [~n_samples:0 ~size:0] records a pure stack-trace point that
    contributes no allocation. This is useful if you are in code that might not
    allocate much and you want to ensure you're still generating enough sample
    points. *)

val create_cursor : string -> int -> Runtime_events.cursor
(** [create_cursor path pid] creates a cursor for reading runtime events
    from the given [path] and [pid]. *)

type sample_point = {
    time: float;
    stack_trace: Stack_trace.t;
    n_samples: int;
    size: int;
    kind: Event.point_kind;
    id: int;
}
(** A single profiling sample. NOTE: the field order is part of the FFI
    contract — the Rust side decodes this as a record with the same field order
    in [src/ocaml_intf.rs], so do not reorder these fields without updating
    that decode.

    [time]: timestamp of sample point
    [stack_trace]: resolved stack trace
    [n_samples]: number of samples associated with the memory block we sampled
    [size]: size of memory block we sampled
    [kind]: whether this was an allocation or deallocation
    [id]: we identify a block by its id so that we only need to send its stack
          trace once during allocation and can cache it and save space on
          deallocation *)

type diagnostics = {
  total_lost_events : int;
  orphan_part_drops : int;
  overflow_part_drops : int;
}
(** Cumulative, monotonically-increasing loss counters surfaced with every
    {!read_poll} so the profiler can log which channel is dropping samples.
    Used to determine if sampling rate is too high.

    [total_lost_events]: events dropped because a per-domain ring buffer
      overflowed before we drained it.
    [orphan_part_drops]: multipart-reassembly drops — a non-start part arrived
      with no start part buffered for its ring.
    [overflow_part_drops]: multipart-reassembly drops — more parts collected
      than [part_count], so the buffer was corrupt (interleaved/duplicated). *)

type read_poll_output = {
    now : float;
    sample_points: sample_point list;
    diagnostics: diagnostics;
}

val read_poll :
  ?max_events:int option -> Runtime_events.cursor -> read_poll_output
(** [read_poll cursor] will read the profiling runtime events from the given
    cursor and return the entire list of {!sample_point} along with the current
    time {!now} and cumulative loss {!diagnostics}. Processing is done by the
    sampler thread that calls this from rust. *)
