module Test_async = Test.Make(Redis_async.Client)
module Test_async_cluster = Test.Make(Redis_async.ClusterClient)

let _ =
  [
    (Test_async.test, "async simple");
    (Test_async_cluster.test, "async cluster")
  ]
  |> List.map (fun (t, name) -> t name)
  |> List.fold_left max 0
  |> Pervasives.exit