let arrayExtend = 
    fun (f: obj) -> 
        fun (xs: obj) ->
            let arr = xs :?> obj[]
            let res = Array.zeroCreate arr.Length
            for i = 0 to arr.Length - 1 do
                res.[i] <- sharpurs_apply f (arr.[i..] :> obj)
            res :> obj
