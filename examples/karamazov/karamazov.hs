
import Control.Monad.Trans
import Control.Monad.Trans.State
import System.IO
import Text.Printf (printf)
import UTF8Wrapper


data RexLastAcc act = 
    RexLccNone
  | RexLccSkip !RexInput
  | RexLcc act !RexInput !Int !RexInput

data RexReturn act = 
    RexError !RexInput
  | RexEOF
  | RexToken act !RexInput !Int !RexInput

rex_action_1 = 
     modify (+ 1) 

-- rexScan :: Int -> RexInput -> RexReturn _
rexScan sc inp0 = 
    case state_0 RexLccNone 0 inp0 of
    (RexLccNone,inp1) -> case rexGetChar 0 inp1 of
        Just {} -> RexError inp1
        Nothing -> RexEOF
    (RexLccSkip inp1,_) -> rexScan sc inp1
    (RexLcc act inp0 len inp1,_) -> RexToken act inp0 len inp1
  where
    state_0 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        shift_0 acc len0 inp0
    
    shift_0 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        case rexGetChar len0 inp0 of
        Just (c,len1,inp1) -> (\next -> next acc len1 inp1) $ case c of
            194 -> state_1
            195 -> state_1
            196 -> state_1
            197 -> state_1
            198 -> state_1
            199 -> state_1
            200 -> state_1
            201 -> state_1
            202 -> state_1
            203 -> state_1
            204 -> state_1
            205 -> state_1
            206 -> state_1
            207 -> state_1
            208 -> state_1
            209 -> state_1
            210 -> state_1
            211 -> state_1
            212 -> state_1
            213 -> state_1
            214 -> state_1
            215 -> state_1
            216 -> state_1
            217 -> state_1
            218 -> state_1
            219 -> state_1
            220 -> state_1
            221 -> state_1
            222 -> state_1
            223 -> state_1
            240 -> state_7
            244 -> state_8
            241 -> state_9
            242 -> state_9
            243 -> state_9
            224 -> state_10
            225 -> state_11
            226 -> state_11
            227 -> state_11
            228 -> state_11
            229 -> state_11
            230 -> state_11
            231 -> state_11
            232 -> state_11
            233 -> state_11
            234 -> state_11
            235 -> state_11
            236 -> state_11
            237 -> state_11
            238 -> state_11
            239 -> state_11
            0 -> state_13
            1 -> state_13
            2 -> state_13
            3 -> state_13
            4 -> state_13
            5 -> state_13
            6 -> state_13
            7 -> state_13
            8 -> state_13
            9 -> state_13
            10 -> state_13
            11 -> state_13
            12 -> state_13
            13 -> state_13
            14 -> state_13
            15 -> state_13
            16 -> state_13
            17 -> state_13
            18 -> state_13
            19 -> state_13
            20 -> state_13
            21 -> state_13
            22 -> state_13
            23 -> state_13
            24 -> state_13
            25 -> state_13
            26 -> state_13
            27 -> state_13
            28 -> state_13
            29 -> state_13
            30 -> state_13
            31 -> state_13
            32 -> state_13
            33 -> state_13
            34 -> state_13
            35 -> state_13
            36 -> state_13
            37 -> state_13
            38 -> state_13
            39 -> state_13
            40 -> state_13
            41 -> state_13
            42 -> state_13
            43 -> state_13
            44 -> state_13
            45 -> state_13
            46 -> state_13
            47 -> state_13
            48 -> state_13
            49 -> state_13
            50 -> state_13
            51 -> state_13
            52 -> state_13
            53 -> state_13
            54 -> state_13
            55 -> state_13
            56 -> state_13
            57 -> state_13
            58 -> state_13
            59 -> state_13
            60 -> state_13
            61 -> state_13
            62 -> state_13
            63 -> state_13
            64 -> state_13
            66 -> state_13
            67 -> state_13
            68 -> state_13
            69 -> state_13
            70 -> state_13
            71 -> state_13
            72 -> state_13
            73 -> state_13
            74 -> state_13
            75 -> state_13
            76 -> state_13
            77 -> state_13
            78 -> state_13
            79 -> state_13
            80 -> state_13
            81 -> state_13
            82 -> state_13
            83 -> state_13
            84 -> state_13
            85 -> state_13
            86 -> state_13
            87 -> state_13
            88 -> state_13
            89 -> state_13
            90 -> state_13
            91 -> state_13
            92 -> state_13
            93 -> state_13
            94 -> state_13
            95 -> state_13
            96 -> state_13
            97 -> state_13
            98 -> state_13
            99 -> state_13
            100 -> state_13
            101 -> state_13
            102 -> state_13
            103 -> state_13
            104 -> state_13
            105 -> state_13
            106 -> state_13
            107 -> state_13
            108 -> state_13
            109 -> state_13
            110 -> state_13
            111 -> state_13
            112 -> state_13
            113 -> state_13
            114 -> state_13
            115 -> state_13
            116 -> state_13
            117 -> state_13
            118 -> state_13
            119 -> state_13
            120 -> state_13
            121 -> state_13
            122 -> state_13
            123 -> state_13
            124 -> state_13
            125 -> state_13
            126 -> state_13
            127 -> state_13
            65 -> state_14
            _ -> \_ _ _ -> (acc,inp0)
        Nothing -> (acc,inp0)
    
    state_1 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        shift_1 acc len0 inp0
    
    shift_1 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        case rexGetChar len0 inp0 of
        Just (c,len1,inp1) -> (\next -> next acc len1 inp1) $ case c of
            128 -> state_13
            129 -> state_13
            130 -> state_13
            131 -> state_13
            132 -> state_13
            133 -> state_13
            134 -> state_13
            135 -> state_13
            136 -> state_13
            137 -> state_13
            138 -> state_13
            139 -> state_13
            140 -> state_13
            141 -> state_13
            142 -> state_13
            143 -> state_13
            144 -> state_13
            145 -> state_13
            146 -> state_13
            147 -> state_13
            148 -> state_13
            149 -> state_13
            150 -> state_13
            151 -> state_13
            152 -> state_13
            153 -> state_13
            154 -> state_13
            155 -> state_13
            156 -> state_13
            157 -> state_13
            158 -> state_13
            159 -> state_13
            160 -> state_13
            161 -> state_13
            162 -> state_13
            163 -> state_13
            164 -> state_13
            165 -> state_13
            166 -> state_13
            167 -> state_13
            168 -> state_13
            169 -> state_13
            170 -> state_13
            171 -> state_13
            172 -> state_13
            173 -> state_13
            174 -> state_13
            175 -> state_13
            176 -> state_13
            177 -> state_13
            178 -> state_13
            179 -> state_13
            180 -> state_13
            181 -> state_13
            182 -> state_13
            183 -> state_13
            184 -> state_13
            185 -> state_13
            186 -> state_13
            187 -> state_13
            188 -> state_13
            189 -> state_13
            190 -> state_13
            191 -> state_13
            _ -> \_ _ _ -> (acc,inp0)
        Nothing -> (acc,inp0)
    
    state_2 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        shift_2 acc len0 inp0
    
    shift_2 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        case rexGetChar len0 inp0 of
        Just (c,len1,inp1) -> (\next -> next acc len1 inp1) $ case c of
            97 -> state_12
            _ -> \_ _ _ -> (acc,inp0)
        Nothing -> (acc,inp0)
    
    state_3 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        shift_3 acc len0 inp0
    
    shift_3 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        case rexGetChar len0 inp0 of
        Just (c,len1,inp1) -> (\next -> next acc len1 inp1) $ case c of
            104 -> state_2
            _ -> \_ _ _ -> (acc,inp0)
        Nothing -> (acc,inp0)
    
    state_4 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        shift_4 acc len0 inp0
    
    shift_4 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        case rexGetChar len0 inp0 of
        Just (c,len1,inp1) -> (\next -> next acc len1 inp1) $ case c of
            115 -> state_3
            _ -> \_ _ _ -> (acc,inp0)
        Nothing -> (acc,inp0)
    
    state_5 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        shift_5 acc len0 inp0
    
    shift_5 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        case rexGetChar len0 inp0 of
        Just (c,len1,inp1) -> (\next -> next acc len1 inp1) $ case c of
            111 -> state_4
            _ -> \_ _ _ -> (acc,inp0)
        Nothing -> (acc,inp0)
    
    state_6 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        shift_6 acc len0 inp0
    
    shift_6 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        case rexGetChar len0 inp0 of
        Just (c,len1,inp1) -> (\next -> next acc len1 inp1) $ case c of
            121 -> state_5
            _ -> \_ _ _ -> (acc,inp0)
        Nothing -> (acc,inp0)
    
    state_7 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        shift_7 acc len0 inp0
    
    shift_7 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        case rexGetChar len0 inp0 of
        Just (c,len1,inp1) -> (\next -> next acc len1 inp1) $ case c of
            144 -> state_11
            145 -> state_11
            146 -> state_11
            147 -> state_11
            148 -> state_11
            149 -> state_11
            150 -> state_11
            151 -> state_11
            152 -> state_11
            153 -> state_11
            154 -> state_11
            155 -> state_11
            156 -> state_11
            157 -> state_11
            158 -> state_11
            159 -> state_11
            160 -> state_11
            161 -> state_11
            162 -> state_11
            163 -> state_11
            164 -> state_11
            165 -> state_11
            166 -> state_11
            167 -> state_11
            168 -> state_11
            169 -> state_11
            170 -> state_11
            171 -> state_11
            172 -> state_11
            173 -> state_11
            174 -> state_11
            175 -> state_11
            176 -> state_11
            177 -> state_11
            178 -> state_11
            179 -> state_11
            180 -> state_11
            181 -> state_11
            182 -> state_11
            183 -> state_11
            184 -> state_11
            185 -> state_11
            186 -> state_11
            187 -> state_11
            188 -> state_11
            189 -> state_11
            190 -> state_11
            191 -> state_11
            _ -> \_ _ _ -> (acc,inp0)
        Nothing -> (acc,inp0)
    
    state_8 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        shift_8 acc len0 inp0
    
    shift_8 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        case rexGetChar len0 inp0 of
        Just (c,len1,inp1) -> (\next -> next acc len1 inp1) $ case c of
            128 -> state_11
            129 -> state_11
            130 -> state_11
            131 -> state_11
            132 -> state_11
            133 -> state_11
            134 -> state_11
            135 -> state_11
            136 -> state_11
            137 -> state_11
            138 -> state_11
            139 -> state_11
            140 -> state_11
            141 -> state_11
            142 -> state_11
            143 -> state_11
            _ -> \_ _ _ -> (acc,inp0)
        Nothing -> (acc,inp0)
    
    state_9 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        shift_9 acc len0 inp0
    
    shift_9 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        case rexGetChar len0 inp0 of
        Just (c,len1,inp1) -> (\next -> next acc len1 inp1) $ case c of
            128 -> state_11
            129 -> state_11
            130 -> state_11
            131 -> state_11
            132 -> state_11
            133 -> state_11
            134 -> state_11
            135 -> state_11
            136 -> state_11
            137 -> state_11
            138 -> state_11
            139 -> state_11
            140 -> state_11
            141 -> state_11
            142 -> state_11
            143 -> state_11
            144 -> state_11
            145 -> state_11
            146 -> state_11
            147 -> state_11
            148 -> state_11
            149 -> state_11
            150 -> state_11
            151 -> state_11
            152 -> state_11
            153 -> state_11
            154 -> state_11
            155 -> state_11
            156 -> state_11
            157 -> state_11
            158 -> state_11
            159 -> state_11
            160 -> state_11
            161 -> state_11
            162 -> state_11
            163 -> state_11
            164 -> state_11
            165 -> state_11
            166 -> state_11
            167 -> state_11
            168 -> state_11
            169 -> state_11
            170 -> state_11
            171 -> state_11
            172 -> state_11
            173 -> state_11
            174 -> state_11
            175 -> state_11
            176 -> state_11
            177 -> state_11
            178 -> state_11
            179 -> state_11
            180 -> state_11
            181 -> state_11
            182 -> state_11
            183 -> state_11
            184 -> state_11
            185 -> state_11
            186 -> state_11
            187 -> state_11
            188 -> state_11
            189 -> state_11
            190 -> state_11
            191 -> state_11
            _ -> \_ _ _ -> (acc,inp0)
        Nothing -> (acc,inp0)
    
    state_10 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        shift_10 acc len0 inp0
    
    shift_10 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        case rexGetChar len0 inp0 of
        Just (c,len1,inp1) -> (\next -> next acc len1 inp1) $ case c of
            160 -> state_1
            161 -> state_1
            162 -> state_1
            163 -> state_1
            164 -> state_1
            165 -> state_1
            166 -> state_1
            167 -> state_1
            168 -> state_1
            169 -> state_1
            170 -> state_1
            171 -> state_1
            172 -> state_1
            173 -> state_1
            174 -> state_1
            175 -> state_1
            176 -> state_1
            177 -> state_1
            178 -> state_1
            179 -> state_1
            180 -> state_1
            181 -> state_1
            182 -> state_1
            183 -> state_1
            184 -> state_1
            185 -> state_1
            186 -> state_1
            187 -> state_1
            188 -> state_1
            189 -> state_1
            190 -> state_1
            191 -> state_1
            _ -> \_ _ _ -> (acc,inp0)
        Nothing -> (acc,inp0)
    
    state_11 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        shift_11 acc len0 inp0
    
    shift_11 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        case rexGetChar len0 inp0 of
        Just (c,len1,inp1) -> (\next -> next acc len1 inp1) $ case c of
            128 -> state_1
            129 -> state_1
            130 -> state_1
            131 -> state_1
            132 -> state_1
            133 -> state_1
            134 -> state_1
            135 -> state_1
            136 -> state_1
            137 -> state_1
            138 -> state_1
            139 -> state_1
            140 -> state_1
            141 -> state_1
            142 -> state_1
            143 -> state_1
            144 -> state_1
            145 -> state_1
            146 -> state_1
            147 -> state_1
            148 -> state_1
            149 -> state_1
            150 -> state_1
            151 -> state_1
            152 -> state_1
            153 -> state_1
            154 -> state_1
            155 -> state_1
            156 -> state_1
            157 -> state_1
            158 -> state_1
            159 -> state_1
            160 -> state_1
            161 -> state_1
            162 -> state_1
            163 -> state_1
            164 -> state_1
            165 -> state_1
            166 -> state_1
            167 -> state_1
            168 -> state_1
            169 -> state_1
            170 -> state_1
            171 -> state_1
            172 -> state_1
            173 -> state_1
            174 -> state_1
            175 -> state_1
            176 -> state_1
            177 -> state_1
            178 -> state_1
            179 -> state_1
            180 -> state_1
            181 -> state_1
            182 -> state_1
            183 -> state_1
            184 -> state_1
            185 -> state_1
            186 -> state_1
            187 -> state_1
            188 -> state_1
            189 -> state_1
            190 -> state_1
            191 -> state_1
            _ -> \_ _ _ -> (acc,inp0)
        Nothing -> (acc,inp0)
    
    state_12 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        (accept_12 len0 inp0, inp0)
    
    state_13 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        (accept_13 len0 inp0, inp0)
    
    state_14 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        shift_14 (accept_14 len0 inp0) len0 inp0
    
    shift_14 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        case rexGetChar len0 inp0 of
        Just (c,len1,inp1) -> (\next -> next acc len1 inp1) $ case c of
            108 -> state_6
            _ -> \_ _ _ -> (acc,inp0)
        Nothing -> (acc,inp0)
    
    accept_12 len inp = 
        case (sc :: Int) of
        0 -> action_1 len inp
        _ -> error "rexScan: undefined start code"
    
    accept_13 len inp = 
        case (sc :: Int) of
        0 -> action_0 len inp
        _ -> error "rexScan: undefined start code"
    
    accept_14 len inp = 
        case (sc :: Int) of
        0 -> action_0 len inp
        _ -> error "rexScan: undefined start code"
    
    action_0 len inp1 = 
        RexLccSkip inp1
    
    action_1 len inp1 = 
        RexLcc rex_action_1 inp0 len inp1
    

seeAlyosha :: RexInput -> StateT Int IO ()
seeAlyosha inp = 
  case rexScan 0 inp of
    RexError (_, txt) -> 
        liftIO $ printf "lexical error: %s\n" (take 10 txt)
    RexEOF -> do
        n <- get 
        liftIO $ printf "'Alyosha' occurs %d times in 'karamazov.txt'\n" n
    RexToken act inp0@(_,_txt) _len inp1 -> do
        act
        seeAlyosha inp1

main :: IO ()
main = 
  withFile "./karamazov.txt" ReadMode $ \h -> do
    inp <- hGetContents h
    evalStateT (seeAlyosha $ fromString inp) 0


