open Async

module Lazy_stream = struct
  type 'a t = unit -> 'a option Deferred.t
  exception Empty

  let from f = f

  let next f =
    f () >>= function
    | Some x -> return x
    | None -> raise Empty
end

module Atomic_channel = struct
  type t = Reader.t Mvar.Read_write.t

  let create reader =
    let mvar = Mvar.create () in
    Mvar.set mvar reader;
    mvar

  let with_reader ch f =
    let read_channel = Mvar.read_only ch in
    Mvar.take read_channel >>= fun reader ->
    f reader >>= fun res ->
    Mvar.put ch reader >>= fun () ->
    return res

  let atomic f ch =
    with_reader ch (fun reader ->
      let nested = create reader in
      f nested) >>= fun x ->
      return x

  let input_char ch =
    with_reader ch @@ fun reader ->
      Reader.read_char reader >>= function
      | `Ok c -> return c
      | `Eof -> raise End_of_file

  let really_input ch buffer pos len =
    with_reader ch @@ fun reader ->
      Reader.really_read reader ~pos ~len buffer >>= function
      | `Ok -> return ()
      | `Eof _ -> raise End_of_file
end

module IO = struct
  type 'a t = 'a Deferred.t

  type in_channel = Atomic_channel.t
  type fd = in_channel * Writer.t
  type out_channel = Writer.t

  type stream_count = unit
  type 'a stream = 'a Lazy_stream.t

  let (>>=) = (>>=)
  let catch f on_error =
    Monitor.try_with ~extract_exn:true f >>= function
    | Ok v -> return v
    | Error exn -> on_error exn

  let try_bind t f g =
    Monitor.try_with ~extract_exn:true t >>= function
    | Ok v -> f v
    | Error ex -> g ex

  let ignore_result d =
    (* not quite correct since it does not pass exception to async_exception_hook *)
    d
    |> Deferred.ignore
    |> Deferred.don't_wait_for

  let return = return
  let fail = raise
  let run d = Thread_safe.block_on_async_exn @@ fun () -> d

  let atomic = Atomic_channel.atomic

  let connect host port =
    let destination = Tcp.to_host_and_port host port in
    Tcp.connect destination >>= fun (_, reader, writer) ->
    return (Atomic_channel.create reader, writer)

  let close (_, writer) = Writer.close writer

  let sleep seconds =
    let span = Core.Time.Span.of_sec seconds in
    after span

  let in_channel_of_descr (reader, _) = reader

  let out_channel_of_descr (_, writer) = writer

  let input_char = Atomic_channel.input_char

  let really_input = Atomic_channel.really_input

  let output_string writer content =
    Writer.write writer content |> return

  let flush = Writer.flushed

  let iter f =
    Deferred.List.iter ~how:`Parallel ~f
  let iter_serial f =
    Deferred.List.iter ~how:`Sequential ~f

  let map f =
    Deferred.List.map ~how:`Parallel ~f

  let map_serial f =
    Deferred.List.map ~how:`Sequential ~f

  let fold_left f init xs =
    Deferred.List.fold ~init ~f xs

  let stream_from = Lazy_stream.from
  let stream_next = Lazy_stream.next
end

module Client = Redis.Client.Make(IO)
module Cache = Redis.Cache.Make(IO)(Client)
module Mutex = Redis.Mutex.Make(IO)(Client)

module ClusterClient = Redis.Client.MakeCluster(IO)
module ClusterCache = Redis.Cache.Make(IO)(ClusterClient)
module ClusterMutex = Redis.Mutex.Make(IO)(ClusterClient)