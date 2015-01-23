#if INTERACTIVE
#else
namespace Underground
#endif

module GreatCircle = 

   open System

    /// Calculates the great-circle distance in km
    /// between two Latitude/Longitude positions on earth.
   let DistanceBetween lat1 long1 lat2 long2 =
      let earthRadius = 6371. // km
      let degToRad (degrees : float) =
           degrees * Math.PI / 180.
      let lat1r, lat2r, long1r, long2r = lat1 |> degToRad, 
                                         lat2 |> degToRad,
                                         long1 |> degToRad,
                                         long2 |> degToRad
      let deltaLat = lat2r - lat1r
      let deltaLong = long2r - long1r

      let a = Math.Sin(deltaLat/2.) ** 2. +
               (Math.Sin(deltaLong/2.) ** 2. * Math.Cos((double)lat1r) * Math.Cos((double)lat2r))

      let c = 2. * Math.Atan2(Math.Sqrt(a), Math.Sqrt(1.-a))

      earthRadius * c
