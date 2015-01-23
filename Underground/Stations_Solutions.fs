#if INTERACTIVE
#else
namespace Underground
#endif

// 2) Make your own function called Pluck takes a function that returns a 
//    bool and an array, and returns a tuple containing of
//    a) the first element for which the function returns true
//    b) all the other elements.
// 
// 3) How could you make use of option types in integrating Pluck?  (Hint - 
//   could you make a TryPluck which returns None if no element returns
//   true, and Some tuple in the happy case?)
//
// Scroll down to see where these are called.
//
module Array =

   /// Returns a tuple containing the first element in
   /// the input array for which f returns true, and
   /// a new array containing all input elements apart
   /// from the selected one.  It is an error for
   /// no element to be be found.
   let pluck f a =
      let hit = Array.find f a
      let remainder = 
         a |> Array.filter (fun x -> x <> hit)
      hit, remainder

   /// Returns a Some tuple containing the first element in
   /// the input array for which f returns true, and
   /// a new array containing all input elements apart
   /// from the selected one.  If no element is found
   /// returns None.
   let tryPluck f a =
      let hit = Array.tryFind f a
      match hit with
      | Some h ->
         let remainder = 
            a |> Array.filter (fun x -> x <> h)
         Some (h, remainder)
      | None -> None

module StationsSolution =

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

// 1) This code will fail with a nasty exception if the stations array is empty.
//    How could you amend the code to meet the following requirements:
//    - No stations provided - result is 0.0
//    - Only one station provided - result is 0.0
//    - Otherwise, behaviour unchanged.
   let VisitAllDistanceExercise1 firstId (stations : StationsData.Row[]) =

      let rec distance acc current remaining =
         match remaining with
         | [||] -> acc
         | _ -> 
            let current', dist = Nearest current remaining
            printfn "Visiting: %s (%0.1fkm)" current'.Name dist
            let acc' = acc + dist
            let remaining' = remaining |> Array.filter (fun s -> s <> current')
            distance acc' current' remaining' 

      // 1)
      if stations.Length > 0 then
         // Could still fail here if no station matched the id:
         let first = stations |> Array.find (fun s -> s.Id = firstId)
         let others = stations |> Array.filter (fun s -> s <> first)
         printfn "Starting at: %s" first.Name
         distance 0. first others
      else
         0.

// 2) Make your own function called Pluck takes a function that returns a 
//    bool and an array, and returns a tuple containing of
//    a) the first element for which the function returns true
//    b) all the other elements.
   let VisitAllDistanceExercise2 firstId (stations : StationsData.Row[]) =

      let rec distance acc current remaining =
         match remaining with
         | [||] -> acc
         | _ -> 
            let current', dist = Nearest current remaining
            printfn "Visiting: %s (%0.1fkm)" current'.Name dist
            let acc' = acc + dist
            let remaining' = remaining |> Array.filter (fun s -> s <> current')
            distance acc' current' remaining' 

      // 2)
      if stations.Length > 0 then
         // Could still fail here if no station matched the id:
         let first = stations |> Array.find (fun s -> s.Id = firstId)
         let others = stations |> Array.filter (fun s -> s <> first)
         printfn "Starting at: %s" first.Name
         distance 0. first others
      else
         0.

// 3) How could you make use of option types in integrating Pluck?  (Hint - 
//    could you make a TryPluck which returns None if no element returns
//    true, and Some tuple in the happy case?)
   let VisitAllDistanceExercise3 firstId (stations : StationsData.Row[]) =

      let rec distance acc current remaining =
         match remaining with
         | [||] -> acc
         | _ -> 
            let current', dist = Nearest current remaining
            printfn "Visiting: %s (%0.1fkm)" current'.Name dist
            let acc' = acc + dist
            let remaining' = remaining |> Array.filter (fun s -> s <> current')
            distance acc' current' remaining' 

      // 3)
      let initialState = stations |> Array.tryPluck (fun s -> s.Id = firstId)
      match initialState with
      | Some (first, others) ->
         printfn "Starting at: %s" first.Name
         distance 0. first others
      | None -> 0.







