#if INTERACTIVE
#else
namespace Underground
#endif

module StationsSimple =

   open System.IO
   open FSharp.Data
   
   type StationsData = CsvProvider< @"c:\data\Stations.csv", HasHeaders=true>

   let Stations fileName =
      use stream = new FileStream(fileName, System.IO.FileMode.Open)
      let stations = StationsData.Load(stream)
      stations.Rows |> Array.ofSeq
   
   // Actually the zone info in the file
   // seems incorrect, but whatever:
   let InZone fileName zone =
      fileName
      |> Stations
      |> Seq.filter (fun r -> r.Zone = zone)
      |> Seq.map (fun r -> r.Name)
      |> Seq.sort
      |> Array.ofSeq

   let ZoneBreakdown fileName =
      fileName
      |> Stations
      |> Seq.map (fun s -> s.Name, s.Zone)
      |> Seq.groupBy (fun (_, z) -> z)
      |> Seq.map (fun (k, v) s -> k, (s |> Seq.length))


