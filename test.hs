main = putStr $ "data=" ++ show dat
  where
    fuee i =
        let s = sin $ i/10
            c = cos $ i/10
            len = 20
            x0 = 400
            y0 = 20
        in [x0+len*c, y0+len*s, x0-len*c, y0-len*s]
    dat = map fuee [0 .. 990]
