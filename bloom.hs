module Main where
    import Data.ByteString (unpack)
    import Data.ByteString.Char8 (pack)
    import Hash ( Hashable(hash32) )  
    main = do
        let x = unpack . pack $ "something"
            in print  $ hash32 x 90 
    
        

