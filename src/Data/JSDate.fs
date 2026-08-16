module Data.JSDate

open System
open System.Collections.Generic

let unixEpoch = DateTimeOffset(1970, 1, 1, 0, 0, 0, TimeSpan.Zero)

let toDouble (date: obj) =
    match date with
    | :? DateTimeOffset as d -> (d - unixEpoch).TotalMilliseconds
    | :? double as d -> d
    | _ -> Double.NaN

let isValid (date: obj) : obj = 
    let ms = toDouble date
    not (Double.IsNaN(ms)) :> obj

let fromInstant (instant: obj) : obj = 
    match instant with
    | :? double as d -> d :> obj
    | _ -> 0.0 :> obj

let toInstantImpl (just: obj) (nothing: obj) (date: obj) : obj =
    let ms = toDouble date
    if Double.IsNaN ms then nothing
    else
        let f = just :?> (obj -> obj)
        f (ms :> obj)

let div (a: float) (b: float) = Math.Floor(a / b)

let isLeapYear y =
    (y % 4.0 = 0.0 && y % 100.0 <> 0.0) || (y % 400.0 = 0.0)

let daysInMonth y m =
    match int m with
    | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31.0
    | 4 | 6 | 9 | 11 -> 30.0
    | 2 -> if isLeapYear y then 29.0 else 28.0
    | _ -> 0.0

let getDaysFrom1970 (y: float) (m: float) (d: float) =
    let m_norm = m % 12.0
    let m_actual = if m_norm < 0.0 then m_norm + 12.0 else m_norm
    let y_actual = y + div m 12.0

    let y_prev = y_actual - 1.0
    let daysToYear = y_prev * 365.0 + div y_prev 4.0 - div y_prev 100.0 + div y_prev 400.0
    
    let mutable daysToMonth = 0.0
    for i = 0 to (int m_actual - 1) do
        daysToMonth <- daysToMonth + daysInMonth y_actual (float (i + 1))
        
    daysToYear + daysToMonth + d - 1.0 - 719162.0

let calculateUTC (y: float) (m: float) (d: float) (h: float) (mi: float) (s: float) (ms: float) =
    let days = getDaysFrom1970 y m d
    days * 86400000.0 + h * 3600000.0 + mi * 60000.0 + s * 1000.0 + ms

let jsdate (parts: obj) : obj =
    let map = parts :?> Map<string, obj>
    try
        let y = Convert.ToDouble(Map.find "year" map)
        let m = Convert.ToDouble(Map.find "month" map)
        let d = Convert.ToDouble(Map.find "day" map)
        let h = Convert.ToDouble(Map.find "hour" map)
        let mi = Convert.ToDouble(Map.find "minute" map)
        let s = Convert.ToDouble(Map.find "second" map)
        let ms = Convert.ToDouble(Map.find "millisecond" map)
        
        if Double.IsNaN(y) || Double.IsNaN(m) || Double.IsNaN(d) || Double.IsNaN(h) || Double.IsNaN(mi) || Double.IsNaN(s) || Double.IsNaN(ms) then
            Double.NaN :> obj
        else
            calculateUTC y m d h mi s ms :> obj
    with _ ->
        Double.NaN :> obj

let jsdateLocal (parts: obj) : obj =
    let eff (u: obj) =
        let map = parts :?> Map<string, obj>
        try
            let y = Convert.ToDouble(Map.find "year" map)
            let m = Convert.ToDouble(Map.find "month" map)
            let d = Convert.ToDouble(Map.find "day" map)
            let h = Convert.ToDouble(Map.find "hour" map)
            let mi = Convert.ToDouble(Map.find "minute" map)
            let s = Convert.ToDouble(Map.find "second" map)
            let ms = Convert.ToDouble(Map.find "millisecond" map)
            
            if Double.IsNaN(y) || Double.IsNaN(m) || Double.IsNaN(d) || Double.IsNaN(h) || Double.IsNaN(mi) || Double.IsNaN(s) || Double.IsNaN(ms) then
                Double.NaN :> obj
            else
                try
                    let mutable date = DateTime(int y, int m + 1, int d, int h, int mi, int s, int ms, DateTimeKind.Local)
                    if y >= 0.0 && y < 100.0 then
                        date <- DateTime(int y, int m + 1, int d, int h, int mi, int s, int ms, DateTimeKind.Local)
                    (new DateTimeOffset(date) - unixEpoch).TotalMilliseconds :> obj
                with _ ->
                    calculateUTC y m d h mi s ms :> obj
        with _ ->
            Double.NaN :> obj
    eff :> obj

let dateMethod (methodName: obj) (date: obj) : obj =
    let method = methodName :?> string
    let ms = toDouble date
    if Double.IsNaN ms then Double.NaN :> obj
    else
        match method with
        | "getTime" -> ms :> obj
        | _ -> 
            try
                let d = unixEpoch.AddMilliseconds(ms)
                match method with
                | "getUTCFullYear" -> float d.UtcDateTime.Year :> obj
                | "getUTCMonth" -> float (d.UtcDateTime.Month - 1) :> obj
                | "getUTCDate" -> float d.UtcDateTime.Day :> obj
                | "getUTCDay" -> float d.UtcDateTime.DayOfWeek :> obj
                | "getUTCHours" -> float d.UtcDateTime.Hour :> obj
                | "getUTCMinutes" -> float d.UtcDateTime.Minute :> obj
                | "getUTCSeconds" -> float d.UtcDateTime.Second :> obj
                | "getUTCMilliseconds" -> float d.UtcDateTime.Millisecond :> obj
                | "getFullYear" -> float d.LocalDateTime.Year :> obj
                | "getMonth" -> float (d.LocalDateTime.Month - 1) :> obj
                | "getDate" -> float d.LocalDateTime.Day :> obj
                | "getDay" -> float d.LocalDateTime.DayOfWeek :> obj
                | "getHours" -> float d.LocalDateTime.Hour :> obj
                | "getMinutes" -> float d.LocalDateTime.Minute :> obj
                | "getSeconds" -> float d.LocalDateTime.Second :> obj
                | "getMilliseconds" -> float d.LocalDateTime.Millisecond :> obj
                | "getTimezoneOffset" -> float d.Offset.TotalMinutes * -1.0 :> obj
                | "toISOString" -> d.UtcDateTime.ToString("yyyy-MM-ddTHH:mm:ss.fffZ") :> obj
                | "toUTCString" -> d.UtcDateTime.ToString("ddd, dd MMM yyyy HH:mm:ss 'GMT'") :> obj
                | "toDateString" -> d.LocalDateTime.ToString("ddd MMM dd yyyy") :> obj
                | "toTimeString" -> d.LocalDateTime.ToString("HH:mm:ss 'GMT'K") :> obj
                | _ -> failwith ("Not implemented method: " + method)
            with _ -> Double.NaN :> obj

let dateMethodEff (methodName: obj) (date: obj) : obj =
    let eff (u: obj) = dateMethod methodName date
    eff :> obj

let parse (dateString: obj) : obj =
    let eff (u: obj) =
        match dateString with
        | :? string as s ->
            let s' = s.Replace("GMT", "")
            match DateTimeOffset.TryParse(s') with
            | (true, dt) -> (dt - unixEpoch).TotalMilliseconds :> obj
            | _ -> Double.NaN :> obj
        | _ -> Double.NaN :> obj
    eff :> obj

let now : obj =
    let eff (u: obj) =
        (DateTimeOffset.UtcNow - unixEpoch).TotalMilliseconds :> obj
    eff :> obj

let fromTime (time: obj) : obj =
    time

