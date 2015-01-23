// To try stuff out:
//
// In Solution Explore open the references node, right click on FSharp.Data and 'Send to interactive'
// Select all code in GreatCircle.fs and send to interactive (ALT-A (CTRL-A on Mac))
// Select all code in Stations.fs and send to interactive (ALT-A (CTRL-A on Mac))
// In FSharp Interactive type:
//
// @"c:\data\stations.csv" |> StationsSolution.Stations |> StationsSolution.VisitAllDistanceExercise1 102;;
// 
#if INTERACTIVE
#else
namespace Underground
#endif

module Stations =

   open System.IO
   open FSharp.Data
   
   /// Important - to make this work copy stations.csv to a suitable
   /// path and amend the string below to match:
   type StationsData = CsvProvider< @"c:\data\Stations.csv", HasHeaders=true>

   let Stations fileName =
      use stream = new FileStream(fileName, System.IO.FileMode.Open)
      let stations = StationsData.Load(stream)
      stations.Rows |> Array.ofSeq

   let Distance (stnA : StationsData.Row) (stnB : StationsData.Row) =
      let (~~) d = d |> float
      GreatCircle.DistanceBetween 
         ~~stnA.Latitude 
         ~~stnA.Longitude 
         ~~stnB.Latitude 
         ~~stnB.Longitude

   let Nearest this others =
      others
      |> Array.map (fun s -> s, Distance this s)
      |> Array.sortBy (fun (_, d) -> d)
      |> Seq.head

   let VisitAllDistance firstId (stations : StationsData.Row[]) =

      let rec distance acc current remaining =
         match remaining with
         | [||] -> acc
         | _ -> 
            let current', dist = Nearest current remaining
            printfn "Visiting: %s (%0.1fkm)" current'.Name dist
            let acc' = acc + dist
            let remaining' = remaining |> Array.filter (fun s -> s <> current')
            distance acc' current' remaining' 

      let first = stations |> Array.find (fun s -> s.Id = firstId)
      let others = stations |> Array.filter (fun s -> s <> first)
      printfn "Starting at: %s" first.Name
      distance 0. first others

//   1) This code will fail with a nasty exception if the stations array is empty.
//      How could you amend the code to meet the following requirements:
//      - No stations provided - result is 0.0
//      - Only one station provided - result is 0.0
//      - Otherwise, behaviour unchanged.
//
//      let first = stations |> Array.find (fun s -> s.Id = firstId)
//      let others = stations |> Array.filter (fun s -> s <> first)
//
//   2) At this moment this code is ugly.  Make your own function called Pluck which
//      encapsulates this behaviour.  I.e. it takes a function that returns a bool
//      and an array, and returns a tuple containing of
//      a) the first element for which the function returns true
//      b) all the other elements.
//
//      let first = stations |> Array.find (fun s -> s.Id = firstId)
//      let others = stations |> Array.filter (fun s -> s <> first)
//
//      Call your new function in place of the ugly code.
//
//   3) How could you make use of option types in integrating Pluck?  (Hint - 
//      could you make a TryPluck which returns None if no element returns
//      true, and Some tuple in the happy case.)






