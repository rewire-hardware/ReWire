module Counter where

import Data.Bits
import Data.Word

data Ctr = C0  | C1  | C2  | C3  | C4  | C5  | C6  | C7  |
           C8  | C9  | C10 | C11 | C12 | C13 | C14 | C15 |
           C16 | C17 | C18 | C19 | C20 | C21 | C22 | C23 |
           C24 | C25 | C26 | C27 | C28 | C29 | C30 | C31 |
           C32 | C33 | C34 | C35 | C36 | C37 | C38 | C39 |
           C40 | C41 | C42 | C43 | C44 | C45 | C46 | C47 |
           C48 | C49 | C50 | C51 | C52 | C53 | C54 | C55 |
           C56 | C57 | C58 | C59 | C60 | C61 | C62 | C63 deriving (Eq,Show)

incCtr :: Ctr -> Ctr
incCtr C0  = C1
incCtr C1  = C2
incCtr C2  = C3
incCtr C3  = C4
incCtr C4  = C5
incCtr C5  = C6
incCtr C6  = C7
incCtr C7  = C8
incCtr C8  = C9
incCtr C9  = C10
incCtr C10 = C11
incCtr C11 = C12
incCtr C12 = C13
incCtr C13 = C14
incCtr C14 = C15
incCtr C15 = C16
incCtr C16 = C17
incCtr C17 = C18
incCtr C18 = C19
incCtr C19 = C20
incCtr C20 = C21
incCtr C21 = C22
incCtr C22 = C23
incCtr C23 = C24
incCtr C24 = C25
incCtr C25 = C26
incCtr C26 = C27
incCtr C27 = C28
incCtr C28 = C29
incCtr C29 = C30
incCtr C30 = C31
incCtr C31 = C32
incCtr C32 = C33
incCtr C33 = C34
incCtr C34 = C35
incCtr C35 = C36
incCtr C36 = C37
incCtr C37 = C38
incCtr C38 = C39
incCtr C39 = C40
incCtr C40 = C41
incCtr C41 = C42
incCtr C42 = C43
incCtr C43 = C44
incCtr C44 = C45
incCtr C45 = C46
incCtr C46 = C47
incCtr C47 = C48
incCtr C48 = C49
incCtr C49 = C50
incCtr C50 = C51
incCtr C51 = C52
incCtr C52 = C53
incCtr C53 = C54
incCtr C54 = C55
incCtr C55 = C56
incCtr C56 = C57
incCtr C57 = C58
incCtr C58 = C59
incCtr C59 = C60
incCtr C60 = C61
incCtr C61 = C62
incCtr C62 = C63
incCtr C63 = C0 

seed :: Ctr -> Word32
seed C0  = 0x428a2f98
seed C1  = 0x71374491
seed C2  = 0xb5c0fbcf
seed C3  = 0xe9b5dba5
seed C4  = 0x3956c25b
seed C5  = 0x59f111f1
seed C6  = 0x923f82a4
seed C7  = 0xab1c5ed5
seed C8  = 0xd807aa98
seed C9  = 0x12835b01
seed C10 = 0x243185be
seed C11 = 0x550c7dc3
seed C12 = 0x72be5d74
seed C13 = 0x80deb1fe
seed C14 = 0x9bdc06a7
seed C15 = 0xc19bf174
seed C16 = 0xe49b69c1
seed C17 = 0xefbe4786
seed C18 = 0x0fc19dc6
seed C19 = 0x240ca1cc
seed C20 = 0x2de92c6f
seed C21 = 0x4a7484aa
seed C22 = 0x5cb0a9dc
seed C23 = 0x76f988da
seed C24 = 0x983e5152
seed C25 = 0xa831c66d
seed C26 = 0xb00327c8
seed C27 = 0xbf597fc7
seed C28 = 0xc6e00bf3
seed C29 = 0xd5a79147
seed C30 = 0x06ca6351
seed C31 = 0x14292967
seed C32 = 0x27b70a85
seed C33 = 0x2e1b2138
seed C34 = 0x4d2c6dfc
seed C35 = 0x53380d13
seed C36 = 0x650a7354
seed C37 = 0x766a0abb
seed C38 = 0x81c2c92e
seed C39 = 0x92722c85
seed C40 = 0xa2bfe8a1
seed C41 = 0xa81a664b
seed C42 = 0xc24b8b70
seed C43 = 0xc76c51a3
seed C44 = 0xd192e819
seed C45 = 0xd6990624
seed C46 = 0xf40e3585
seed C47 = 0x106aa070
seed C48 = 0x19a4c116
seed C49 = 0x1e376c08
seed C50 = 0x2748774c
seed C51 = 0x34b0bcb5
seed C52 = 0x391c0cb3
seed C53 = 0x4ed8aa4a
seed C54 = 0x5b9cca4f
seed C55 = 0x682e6ff3
seed C56 = 0x748f82ee
seed C57 = 0x78a5636f
seed C58 = 0x84c87814
seed C59 = 0x8cc70208
seed C60 = 0x90befffa
seed C61 = 0xa4506ceb
seed C62 = 0xbef9a3f7
seed C63 = 0xc67178f2

