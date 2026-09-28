
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
     () 

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
            48 -> state_2
            49 -> state_2
            50 -> state_2
            51 -> state_2
            52 -> state_2
            53 -> state_2
            54 -> state_2
            55 -> state_2
            56 -> state_2
            57 -> state_2
            0 -> state_3
            1 -> state_3
            2 -> state_3
            3 -> state_3
            4 -> state_3
            5 -> state_3
            6 -> state_3
            7 -> state_3
            8 -> state_3
            9 -> state_3
            10 -> state_3
            11 -> state_3
            12 -> state_3
            13 -> state_3
            14 -> state_3
            15 -> state_3
            16 -> state_3
            17 -> state_3
            18 -> state_3
            19 -> state_3
            20 -> state_3
            21 -> state_3
            22 -> state_3
            23 -> state_3
            24 -> state_3
            25 -> state_3
            26 -> state_3
            27 -> state_3
            28 -> state_3
            29 -> state_3
            30 -> state_3
            31 -> state_3
            32 -> state_3
            33 -> state_3
            34 -> state_3
            35 -> state_3
            36 -> state_3
            37 -> state_3
            38 -> state_3
            39 -> state_3
            40 -> state_3
            41 -> state_3
            42 -> state_3
            43 -> state_3
            44 -> state_3
            45 -> state_3
            46 -> state_3
            47 -> state_3
            58 -> state_3
            59 -> state_3
            60 -> state_3
            61 -> state_3
            62 -> state_3
            63 -> state_3
            64 -> state_3
            65 -> state_3
            66 -> state_3
            67 -> state_3
            68 -> state_3
            69 -> state_3
            70 -> state_3
            71 -> state_3
            72 -> state_3
            73 -> state_3
            74 -> state_3
            75 -> state_3
            76 -> state_3
            77 -> state_3
            78 -> state_3
            79 -> state_3
            80 -> state_3
            81 -> state_3
            82 -> state_3
            83 -> state_3
            84 -> state_3
            85 -> state_3
            86 -> state_3
            87 -> state_3
            88 -> state_3
            89 -> state_3
            90 -> state_3
            91 -> state_3
            92 -> state_3
            93 -> state_3
            94 -> state_3
            95 -> state_3
            96 -> state_3
            97 -> state_3
            98 -> state_3
            99 -> state_3
            100 -> state_3
            101 -> state_3
            102 -> state_3
            103 -> state_3
            104 -> state_3
            105 -> state_3
            106 -> state_3
            107 -> state_3
            108 -> state_3
            109 -> state_3
            110 -> state_3
            111 -> state_3
            112 -> state_3
            113 -> state_3
            114 -> state_3
            115 -> state_3
            116 -> state_3
            117 -> state_3
            118 -> state_3
            119 -> state_3
            120 -> state_3
            121 -> state_3
            122 -> state_3
            123 -> state_3
            124 -> state_3
            125 -> state_3
            126 -> state_3
            127 -> state_3
            225 -> state_8
            226 -> state_8
            227 -> state_8
            228 -> state_8
            229 -> state_8
            230 -> state_8
            231 -> state_8
            232 -> state_8
            233 -> state_8
            234 -> state_8
            235 -> state_8
            236 -> state_8
            237 -> state_8
            238 -> state_8
            239 -> state_8
            224 -> state_9
            241 -> state_10
            242 -> state_10
            243 -> state_10
            244 -> state_11
            240 -> state_12
            194 -> state_15
            195 -> state_15
            196 -> state_15
            197 -> state_15
            198 -> state_15
            199 -> state_15
            200 -> state_15
            201 -> state_15
            202 -> state_15
            203 -> state_15
            204 -> state_15
            205 -> state_15
            206 -> state_15
            207 -> state_15
            208 -> state_15
            209 -> state_15
            210 -> state_15
            211 -> state_15
            212 -> state_15
            213 -> state_15
            214 -> state_15
            215 -> state_15
            216 -> state_15
            217 -> state_15
            218 -> state_15
            219 -> state_15
            220 -> state_15
            221 -> state_15
            222 -> state_15
            223 -> state_15
            _ -> \_ _ _ -> (acc,inp0)
        Nothing -> (acc,inp0)
    
    state_1 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        shift_1 (accept_1 len0 inp0) len0 inp0
    
    shift_1 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        case rexGetChar len0 inp0 of
        Just (c,len1,inp1) -> (\next -> next acc len1 inp1) $ case c of
            48 -> state_6
            49 -> state_6
            50 -> state_6
            51 -> state_6
            52 -> state_6
            53 -> state_6
            54 -> state_6
            55 -> state_6
            56 -> state_6
            57 -> state_6
            44 -> state_13
            _ -> \_ _ _ -> (acc,inp0)
        Nothing -> (acc,inp0)
    
    state_2 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        shift_2 (accept_2 len0 inp0) len0 inp0
    
    shift_2 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        case rexGetChar len0 inp0 of
        Just (c,len1,inp1) -> (\next -> next acc len1 inp1) $ case c of
            48 -> state_1
            49 -> state_1
            50 -> state_1
            51 -> state_1
            52 -> state_1
            53 -> state_1
            54 -> state_1
            55 -> state_1
            56 -> state_1
            57 -> state_1
            44 -> state_13
            _ -> \_ _ _ -> (acc,inp0)
        Nothing -> (acc,inp0)
    
    state_3 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        shift_3 (accept_3 len0 inp0) len0 inp0
    
    shift_3 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        case rexGetChar len0 inp0 of
        Just (c,len1,inp1) -> (\next -> next acc len1 inp1) $ case c of
            0 -> state_3
            1 -> state_3
            2 -> state_3
            3 -> state_3
            4 -> state_3
            5 -> state_3
            6 -> state_3
            7 -> state_3
            8 -> state_3
            9 -> state_3
            10 -> state_3
            11 -> state_3
            12 -> state_3
            13 -> state_3
            14 -> state_3
            15 -> state_3
            16 -> state_3
            17 -> state_3
            18 -> state_3
            19 -> state_3
            20 -> state_3
            21 -> state_3
            22 -> state_3
            23 -> state_3
            24 -> state_3
            25 -> state_3
            26 -> state_3
            27 -> state_3
            28 -> state_3
            29 -> state_3
            30 -> state_3
            31 -> state_3
            32 -> state_3
            33 -> state_3
            34 -> state_3
            35 -> state_3
            36 -> state_3
            37 -> state_3
            38 -> state_3
            39 -> state_3
            40 -> state_3
            41 -> state_3
            42 -> state_3
            43 -> state_3
            44 -> state_3
            45 -> state_3
            46 -> state_3
            47 -> state_3
            58 -> state_3
            59 -> state_3
            60 -> state_3
            61 -> state_3
            62 -> state_3
            63 -> state_3
            64 -> state_3
            65 -> state_3
            66 -> state_3
            67 -> state_3
            68 -> state_3
            69 -> state_3
            70 -> state_3
            71 -> state_3
            72 -> state_3
            73 -> state_3
            74 -> state_3
            75 -> state_3
            76 -> state_3
            77 -> state_3
            78 -> state_3
            79 -> state_3
            80 -> state_3
            81 -> state_3
            82 -> state_3
            83 -> state_3
            84 -> state_3
            85 -> state_3
            86 -> state_3
            87 -> state_3
            88 -> state_3
            89 -> state_3
            90 -> state_3
            91 -> state_3
            92 -> state_3
            93 -> state_3
            94 -> state_3
            95 -> state_3
            96 -> state_3
            97 -> state_3
            98 -> state_3
            99 -> state_3
            100 -> state_3
            101 -> state_3
            102 -> state_3
            103 -> state_3
            104 -> state_3
            105 -> state_3
            106 -> state_3
            107 -> state_3
            108 -> state_3
            109 -> state_3
            110 -> state_3
            111 -> state_3
            112 -> state_3
            113 -> state_3
            114 -> state_3
            115 -> state_3
            116 -> state_3
            117 -> state_3
            118 -> state_3
            119 -> state_3
            120 -> state_3
            121 -> state_3
            122 -> state_3
            123 -> state_3
            124 -> state_3
            125 -> state_3
            126 -> state_3
            127 -> state_3
            225 -> state_8
            226 -> state_8
            227 -> state_8
            228 -> state_8
            229 -> state_8
            230 -> state_8
            231 -> state_8
            232 -> state_8
            233 -> state_8
            234 -> state_8
            235 -> state_8
            236 -> state_8
            237 -> state_8
            238 -> state_8
            239 -> state_8
            224 -> state_9
            241 -> state_10
            242 -> state_10
            243 -> state_10
            244 -> state_11
            240 -> state_12
            194 -> state_15
            195 -> state_15
            196 -> state_15
            197 -> state_15
            198 -> state_15
            199 -> state_15
            200 -> state_15
            201 -> state_15
            202 -> state_15
            203 -> state_15
            204 -> state_15
            205 -> state_15
            206 -> state_15
            207 -> state_15
            208 -> state_15
            209 -> state_15
            210 -> state_15
            211 -> state_15
            212 -> state_15
            213 -> state_15
            214 -> state_15
            215 -> state_15
            216 -> state_15
            217 -> state_15
            218 -> state_15
            219 -> state_15
            220 -> state_15
            221 -> state_15
            222 -> state_15
            223 -> state_15
            _ -> \_ _ _ -> (acc,inp0)
        Nothing -> (acc,inp0)
    
    state_4 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        shift_4 (accept_4 len0 inp0) len0 inp0
    
    shift_4 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        case rexGetChar len0 inp0 of
        Just (c,len1,inp1) -> (\next -> next acc len1 inp1) $ case c of
            48 -> state_4
            49 -> state_4
            50 -> state_4
            51 -> state_4
            52 -> state_4
            53 -> state_4
            54 -> state_4
            55 -> state_4
            56 -> state_4
            57 -> state_4
            _ -> \_ _ _ -> (acc,inp0)
        Nothing -> (acc,inp0)
    
    state_5 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        shift_5 (accept_5 len0 inp0) len0 inp0
    
    shift_5 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        case rexGetChar len0 inp0 of
        Just (c,len1,inp1) -> (\next -> next acc len1 inp1) $ case c of
            44 -> state_13
            _ -> \_ _ _ -> (acc,inp0)
        Nothing -> (acc,inp0)
    
    state_6 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        shift_6 acc len0 inp0
    
    shift_6 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        case rexGetChar len0 inp0 of
        Just (c,len1,inp1) -> (\next -> next acc len1 inp1) $ case c of
            48 -> state_4
            49 -> state_4
            50 -> state_4
            51 -> state_4
            52 -> state_4
            53 -> state_4
            54 -> state_4
            55 -> state_4
            56 -> state_4
            57 -> state_4
            44 -> state_13
            _ -> \_ _ _ -> (acc,inp0)
        Nothing -> (acc,inp0)
    
    state_7 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        shift_7 acc len0 inp0
    
    shift_7 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        case rexGetChar len0 inp0 of
        Just (c,len1,inp1) -> (\next -> next acc len1 inp1) $ case c of
            48 -> state_5
            49 -> state_5
            50 -> state_5
            51 -> state_5
            52 -> state_5
            53 -> state_5
            54 -> state_5
            55 -> state_5
            56 -> state_5
            57 -> state_5
            _ -> \_ _ _ -> (acc,inp0)
        Nothing -> (acc,inp0)
    
    state_8 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        shift_8 acc len0 inp0
    
    shift_8 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        case rexGetChar len0 inp0 of
        Just (c,len1,inp1) -> (\next -> next acc len1 inp1) $ case c of
            128 -> state_15
            129 -> state_15
            130 -> state_15
            131 -> state_15
            132 -> state_15
            133 -> state_15
            134 -> state_15
            135 -> state_15
            136 -> state_15
            137 -> state_15
            138 -> state_15
            139 -> state_15
            140 -> state_15
            141 -> state_15
            142 -> state_15
            143 -> state_15
            144 -> state_15
            145 -> state_15
            146 -> state_15
            147 -> state_15
            148 -> state_15
            149 -> state_15
            150 -> state_15
            151 -> state_15
            152 -> state_15
            153 -> state_15
            154 -> state_15
            155 -> state_15
            156 -> state_15
            157 -> state_15
            158 -> state_15
            159 -> state_15
            160 -> state_15
            161 -> state_15
            162 -> state_15
            163 -> state_15
            164 -> state_15
            165 -> state_15
            166 -> state_15
            167 -> state_15
            168 -> state_15
            169 -> state_15
            170 -> state_15
            171 -> state_15
            172 -> state_15
            173 -> state_15
            174 -> state_15
            175 -> state_15
            176 -> state_15
            177 -> state_15
            178 -> state_15
            179 -> state_15
            180 -> state_15
            181 -> state_15
            182 -> state_15
            183 -> state_15
            184 -> state_15
            185 -> state_15
            186 -> state_15
            187 -> state_15
            188 -> state_15
            189 -> state_15
            190 -> state_15
            191 -> state_15
            _ -> \_ _ _ -> (acc,inp0)
        Nothing -> (acc,inp0)
    
    state_9 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        shift_9 acc len0 inp0
    
    shift_9 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        case rexGetChar len0 inp0 of
        Just (c,len1,inp1) -> (\next -> next acc len1 inp1) $ case c of
            160 -> state_15
            161 -> state_15
            162 -> state_15
            163 -> state_15
            164 -> state_15
            165 -> state_15
            166 -> state_15
            167 -> state_15
            168 -> state_15
            169 -> state_15
            170 -> state_15
            171 -> state_15
            172 -> state_15
            173 -> state_15
            174 -> state_15
            175 -> state_15
            176 -> state_15
            177 -> state_15
            178 -> state_15
            179 -> state_15
            180 -> state_15
            181 -> state_15
            182 -> state_15
            183 -> state_15
            184 -> state_15
            185 -> state_15
            186 -> state_15
            187 -> state_15
            188 -> state_15
            189 -> state_15
            190 -> state_15
            191 -> state_15
            _ -> \_ _ _ -> (acc,inp0)
        Nothing -> (acc,inp0)
    
    state_10 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        shift_10 acc len0 inp0
    
    shift_10 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        case rexGetChar len0 inp0 of
        Just (c,len1,inp1) -> (\next -> next acc len1 inp1) $ case c of
            128 -> state_8
            129 -> state_8
            130 -> state_8
            131 -> state_8
            132 -> state_8
            133 -> state_8
            134 -> state_8
            135 -> state_8
            136 -> state_8
            137 -> state_8
            138 -> state_8
            139 -> state_8
            140 -> state_8
            141 -> state_8
            142 -> state_8
            143 -> state_8
            144 -> state_8
            145 -> state_8
            146 -> state_8
            147 -> state_8
            148 -> state_8
            149 -> state_8
            150 -> state_8
            151 -> state_8
            152 -> state_8
            153 -> state_8
            154 -> state_8
            155 -> state_8
            156 -> state_8
            157 -> state_8
            158 -> state_8
            159 -> state_8
            160 -> state_8
            161 -> state_8
            162 -> state_8
            163 -> state_8
            164 -> state_8
            165 -> state_8
            166 -> state_8
            167 -> state_8
            168 -> state_8
            169 -> state_8
            170 -> state_8
            171 -> state_8
            172 -> state_8
            173 -> state_8
            174 -> state_8
            175 -> state_8
            176 -> state_8
            177 -> state_8
            178 -> state_8
            179 -> state_8
            180 -> state_8
            181 -> state_8
            182 -> state_8
            183 -> state_8
            184 -> state_8
            185 -> state_8
            186 -> state_8
            187 -> state_8
            188 -> state_8
            189 -> state_8
            190 -> state_8
            191 -> state_8
            _ -> \_ _ _ -> (acc,inp0)
        Nothing -> (acc,inp0)
    
    state_11 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        shift_11 acc len0 inp0
    
    shift_11 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        case rexGetChar len0 inp0 of
        Just (c,len1,inp1) -> (\next -> next acc len1 inp1) $ case c of
            128 -> state_8
            129 -> state_8
            130 -> state_8
            131 -> state_8
            132 -> state_8
            133 -> state_8
            134 -> state_8
            135 -> state_8
            136 -> state_8
            137 -> state_8
            138 -> state_8
            139 -> state_8
            140 -> state_8
            141 -> state_8
            142 -> state_8
            143 -> state_8
            _ -> \_ _ _ -> (acc,inp0)
        Nothing -> (acc,inp0)
    
    state_12 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        shift_12 acc len0 inp0
    
    shift_12 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        case rexGetChar len0 inp0 of
        Just (c,len1,inp1) -> (\next -> next acc len1 inp1) $ case c of
            144 -> state_8
            145 -> state_8
            146 -> state_8
            147 -> state_8
            148 -> state_8
            149 -> state_8
            150 -> state_8
            151 -> state_8
            152 -> state_8
            153 -> state_8
            154 -> state_8
            155 -> state_8
            156 -> state_8
            157 -> state_8
            158 -> state_8
            159 -> state_8
            160 -> state_8
            161 -> state_8
            162 -> state_8
            163 -> state_8
            164 -> state_8
            165 -> state_8
            166 -> state_8
            167 -> state_8
            168 -> state_8
            169 -> state_8
            170 -> state_8
            171 -> state_8
            172 -> state_8
            173 -> state_8
            174 -> state_8
            175 -> state_8
            176 -> state_8
            177 -> state_8
            178 -> state_8
            179 -> state_8
            180 -> state_8
            181 -> state_8
            182 -> state_8
            183 -> state_8
            184 -> state_8
            185 -> state_8
            186 -> state_8
            187 -> state_8
            188 -> state_8
            189 -> state_8
            190 -> state_8
            191 -> state_8
            _ -> \_ _ _ -> (acc,inp0)
        Nothing -> (acc,inp0)
    
    state_13 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        shift_13 acc len0 inp0
    
    shift_13 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        case rexGetChar len0 inp0 of
        Just (c,len1,inp1) -> (\next -> next acc len1 inp1) $ case c of
            48 -> state_14
            49 -> state_14
            50 -> state_14
            51 -> state_14
            52 -> state_14
            53 -> state_14
            54 -> state_14
            55 -> state_14
            56 -> state_14
            57 -> state_14
            _ -> \_ _ _ -> (acc,inp0)
        Nothing -> (acc,inp0)
    
    state_14 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        shift_14 acc len0 inp0
    
    shift_14 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        case rexGetChar len0 inp0 of
        Just (c,len1,inp1) -> (\next -> next acc len1 inp1) $ case c of
            48 -> state_7
            49 -> state_7
            50 -> state_7
            51 -> state_7
            52 -> state_7
            53 -> state_7
            54 -> state_7
            55 -> state_7
            56 -> state_7
            57 -> state_7
            _ -> \_ _ _ -> (acc,inp0)
        Nothing -> (acc,inp0)
    
    state_15 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        shift_15 acc len0 inp0
    
    shift_15 acc len0 inp0 = acc `seq` len0 `seq` inp0 `seq`
        case rexGetChar len0 inp0 of
        Just (c,len1,inp1) -> (\next -> next acc len1 inp1) $ case c of
            128 -> state_3
            129 -> state_3
            130 -> state_3
            131 -> state_3
            132 -> state_3
            133 -> state_3
            134 -> state_3
            135 -> state_3
            136 -> state_3
            137 -> state_3
            138 -> state_3
            139 -> state_3
            140 -> state_3
            141 -> state_3
            142 -> state_3
            143 -> state_3
            144 -> state_3
            145 -> state_3
            146 -> state_3
            147 -> state_3
            148 -> state_3
            149 -> state_3
            150 -> state_3
            151 -> state_3
            152 -> state_3
            153 -> state_3
            154 -> state_3
            155 -> state_3
            156 -> state_3
            157 -> state_3
            158 -> state_3
            159 -> state_3
            160 -> state_3
            161 -> state_3
            162 -> state_3
            163 -> state_3
            164 -> state_3
            165 -> state_3
            166 -> state_3
            167 -> state_3
            168 -> state_3
            169 -> state_3
            170 -> state_3
            171 -> state_3
            172 -> state_3
            173 -> state_3
            174 -> state_3
            175 -> state_3
            176 -> state_3
            177 -> state_3
            178 -> state_3
            179 -> state_3
            180 -> state_3
            181 -> state_3
            182 -> state_3
            183 -> state_3
            184 -> state_3
            185 -> state_3
            186 -> state_3
            187 -> state_3
            188 -> state_3
            189 -> state_3
            190 -> state_3
            191 -> state_3
            _ -> \_ _ _ -> (acc,inp0)
        Nothing -> (acc,inp0)
    
    accept_1 len inp = 
        case (sc :: Int) of
        0 -> action_0 len inp
        _ -> error "rexScan: undefined start code"
    
    accept_2 len inp = 
        case (sc :: Int) of
        0 -> action_0 len inp
        _ -> error "rexScan: undefined start code"
    
    accept_3 len inp = 
        case (sc :: Int) of
        0 -> action_0 len inp
        _ -> error "rexScan: undefined start code"
    
    accept_4 len inp = 
        case (sc :: Int) of
        0 -> action_1 len inp
        _ -> error "rexScan: undefined start code"
    
    accept_5 len inp = 
        case (sc :: Int) of
        0 -> action_1 len inp
        _ -> error "rexScan: undefined start code"
    
    action_0 len inp1 = 
        RexLccSkip inp1
    
    action_1 len inp1 = 
        RexLcc rex_action_1 inp0 len inp1
    

seeBignums :: RexInput -> IO ()
seeBignums inp = 
    case rexScan 0 inp of
    RexError (_, txt) -> 
        printf "lexical error: %s\n" (take 10 txt)
    RexEOF -> 
        pure ()
    RexToken () inp0@(_, txt) len inp1 -> do
        printf "Saw %s\n" (take len txt)
        seeBignums inp1

main :: IO ()
main = do
    inp <- getContents
    seeBignums $ fromString inp


