module WebServices

open WebReferences

type WebReferences.WeatherForecast with
    member ws.GetWeatherByPlaceNameAsyncr placeName =
        Async.FromBeginEnd(placeName,
                           ws.BeginGetWeatherByPlaceName,
                           ws.EndGetWeatherByPlaceName)

type WebReferences.TerraService with
    member ws.GetPlaceFactsAsyncr place =
        Async.FromBeginEnd(place,
                           ws.BeginGetPlaceFacts,
                           ws.EndGetPlaceFacts)

type Microsoft.FSharp.Control.Async with
    static member Parallel2 (a1, a2) =
        async { let! job1 = Async.StartChild a1
                let! job2 = Async.StartChild a2
                let! res1 = job1
                let! res2 = job2
                return res1, res2 }

let getWeather(city,state,country) =
    async { let ws = new WeatherForecast()
            let ts = new TerraService()
            let place = new Place(City=city, State=state, Country=country)
            let! weather,facts =
                Async.Parallel2 (
                    ws.GetWeatherByPlaceNameAsyncr(city),
                    ts.GetPlaceFactsAsyncr(place))
            let today = weather.Details.[0]
            return (today.MinTemperatureF,today.MaxTemperatureC,
                    facts.Center.Lat,facts.Center.Lon) }

let _ =
    Async.RunSynchronously (
        async { let! (maxF,maxC,lat,lon) = getWeather("Los Angeles","CA","USA")
                do printfn "Temperature: %sF/%sC" maxF maxC
                do printfn "Lat/Lon: %f/%f" lat lon })
