module Main where
    import Func.Definiciones

    lista = repetir [3,4,3]
    ejemplo1 = coger 1 [3,4,3,5]
    ejemplo2 = coger 2 [3,4,3,5]
    ejemplo3 = coger 3 [3,4,3,5]

    main = do
        print ejemplo1
        print ejemplo2
        print ejemplo3
        print lista