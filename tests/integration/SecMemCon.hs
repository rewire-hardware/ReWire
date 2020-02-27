import ReWire
import ReWire.Bits

data W2  = W2 Bit Bit
data W10 = W10
      { b0 :: Bit
      , b1 :: Bit
      , b2 :: Bit
      , b3 :: Bit
      , b4 :: Bit
      , b5 :: Bit
      , b6 :: Bit
      , b7 :: Bit
      , b8 :: Bit
      , b9 :: Bit
      }

-- TODO(chathhorn)
data Mem = Mem
      { mem0 :: W32
      , mem1 :: W32
      , mem2 :: W32
      , mem3 :: W32
      }

zeroMem :: Mem
zeroMem = Mem
      { mem0 = zeroW32
      , mem1 = zeroW32
      , mem2 = zeroW32
      , mem3 = zeroW32
      }

memLookup :: W10 -> Mem -> W32
memLookup (W10 C C C C C C C C C C) = mem0
memLookup (W10 C C C C C C C C C S) = mem1
memLookup (W10 C C C C C C C C S C) = mem2
memLookup (W10 C C C C C C C C S S) = mem3

memTweak :: W10 -> W32 -> Mem -> Mem
memTweak (W10 C C C C C C C C C C) v m = m { mem0 = v }
memTweak (W10 C C C C C C C C C S) v m = m { mem1 = v }
memTweak (W10 C C C C C C C C S C) v m = m { mem2 = v }
memTweak (W10 C C C C C C C C S S) v m = m { mem3 = v }

-- replaces the first two bits of its second argument with its first argument
(<&>) :: W2 -> W10 -> W10
(W2 b0 b1) <&> (W10 _ _ b2 b3 b4 b5 b6 b7 b8 b9) = W10 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9

-- Some constants
zeroW2 :: W2
zeroW2  = W2 C C

zeroW10 :: W10
zeroW10 = W10 C C C C C C C C C C

onesW10 :: W10
onesW10 = W10 S S S S S S S S S S

decW10 :: W10 -> W10
decW10 w@W10 { b0 = S } = w { b0 = C }
decW10 w@W10 { b1 = S, b0 = C } = w { b1 = C, b0 = S }
decW10 w@W10 { b2 = S, b1 = C, b0 = C } = w { b2 = C, b1 = S, b0 = S }
decW10 w@W10 { b3 = S, b2 = C, b1 = C, b0 = C } = w { b3 = C, b2 = S, b1 = S, b0 = S }
decW10 w@W10 { b4 = S, b3 = C, b2 = C, b1 = C, b0 = C } = w { b4 = C, b3 = S, b2 = S, b1 = S, b0 = S }
decW10 w@W10 { b5 = S, b4 = C, b3 = C, b2 = C, b1 = C, b0 = C } = w { b5 = C, b4 = S, b3 = S, b2 = S, b1 = S, b0 = S }
decW10 w@W10 { b6 = S, b5 = C, b4 = C, b3 = C, b2 = C, b1 = C, b0 = C } = w { b6 = C, b5 = S, b4 = S, b3 = S, b2 = S, b1 = S, b0 = S }
decW10 w@W10 { b7 = S, b6 = C, b5 = C, b4 = C, b3 = C, b2 = C, b1 = C, b0 = C } = w { b7 = C, b6 = S, b5 = S, b4 = S, b3 = S, b2 = S, b1 = S, b0 = S }
-- This last case would be wrapping a signed value.
decW10 w@W10 { b9 = S, b8 = C, b7 = C, b6 = C, b5 = C, b4 = C, b3 = C, b2 = C, b1 = C, b0 = C } = w { b9 = C, b8 = S, b7 = S, b6 = S, b5 = S, b4 = S, b3 = S, b2 = S, b1 = S, b0 = S }

positiveW10 :: W10 -> Bool
positiveW10 W10 { b9 = S } = True
positiveW10 _              = False

--------------------------------------------
-- Types for Signals, Ports, & Memory     --
--------------------------------------------

data Sigs = Sigs
  { partition_reg :: W2  {- [0..1] -}
  , addr_reg      :: W10 {- [0..9] -}
  , data_reg      :: W32 {- [0..31] -}
  , ack_reg       :: Bit
  , data_out_reg  :: W32 {- [0..31] -}
  }

data PortsIn = PortsIn
  { data_in       :: W32 {- [0..31] -}
  , addr_in       :: W10 {- [0..9] -}
  , go            :: Bit
  , rnw           :: Bit
  , partition_in  :: W2 {- [0..1] -}
  }

data PortsOut = PortsOut
  { ack_out  :: Bit
  , data_out :: W32 {- [0..31] -}
  }

zeroSigs :: Sigs
zeroSigs = Sigs
      { partition_reg = zeroW2
      , addr_reg      = zeroW10
      , data_reg      = zeroW32
      , ack_reg       = C
      , data_out_reg  = zeroW32
      }

---------------------
-- Some Helpers
---------------------

set_partition_reg :: W2 -> StT (Sigs, Mem) I ()
set_partition_reg w2 = modify (\ (s, m) -> (s { partition_reg = w2 }, m))

get_partition_reg :: StT (Sigs, Mem) I W2
get_partition_reg = get >>= \ (s, _) -> return (partition_reg s)

set_ack_reg :: Bit -> StT (Sigs, Mem) I ()
set_ack_reg b = modify (\ (s, m) -> (s { ack_reg = b }, m))

get_ack_reg :: StT (Sigs, Mem) I Bit
get_ack_reg = get >>= \ (s, _) -> return (ack_reg s)

set_data_reg :: W32 -> StT (Sigs, Mem) I ()
set_data_reg w32 = modify (\ (s, m) -> (s { data_reg = w32 }, m))

get_data_reg :: StT (Sigs, Mem) I W32
get_data_reg = get >>= \ (s, _) -> return (data_reg s)

set_data_out_reg :: W32 -> StT (Sigs, Mem) I ()
set_data_out_reg w32 = modify (\ (s, m) -> (s { data_out_reg = w32 }, m))

get_data_out_reg :: StT (Sigs, Mem) I W32
get_data_out_reg = get >>= \ (s, _) -> return (data_out_reg s)

set_addr_reg :: W10 -> StT (Sigs, Mem) I ()
set_addr_reg w10 = modify (\ (s, m) -> (s { addr_reg = w10 }, m))

get_addr_reg :: StT (Sigs, Mem) I W10
get_addr_reg = get >>= \ (s, _) -> return (addr_reg s)

-- Memory operations
getloc :: W10 -> StT (Sigs, Mem) I W32
getloc a = get >>= \ (_, mem) -> return (memLookup a mem)

setloc :: W10 -> W32 -> StT (Sigs, Mem) I ()
setloc i e = modify (\ (s, m) -> (s, memTweak i e m))

------------------------------------------
------------------------------------------
------------------------------------------
------------------------------------------
------------------------------------------
-----      End of ReWire figleaf     -----
------------------------------------------
------------------------------------------
------------------------------------------
------------------------------------------
------------------------------------------

connect :: (Sigs, Mem) -> ReT PortsIn PortsOut (StT (Sigs, Mem) I) PortsIn
connect x = do
      lift $ put x
      signal $ outports $ fst x
  where
    outports :: Sigs -> PortsOut
    outports s = PortsOut { ack_out = ack_reg s , data_out = data_out_reg s }

reset :: ReT PortsIn PortsOut (StT (Sigs, Mem) I) ()
reset = do
      ports <- (lift (reset_act >> get) >>= connect)
      scrub_ram ports
  where
    reset_act = do
      set_ack_reg C
      set_data_reg zeroW32
      set_data_out_reg zeroW32
      set_partition_reg zeroW2
      set_addr_reg onesW10
      setloc zeroW10 zeroW32

scrub_ram :: PortsIn -> ReT PortsIn PortsOut (StT (Sigs, Mem) I) ()
scrub_ram ips = (lift (scrub_ram_act >> get) >>= while_addr_reg_0) >>= idle
  where
    scrub_ram_act :: StT (Sigs, Mem) I ()
    scrub_ram_act = do
      addr <- get_addr_reg
      setloc addr zeroW32
      set_addr_reg (decW10 addr)

while_addr_reg_0
      :: (Sigs, Mem)
      -> ReT PortsIn PortsOut (StT (Sigs, Mem) I) PortsIn
while_addr_reg_0 x = do
  addr <- lift get_addr_reg
  if positiveW10 addr
    then do
      connect x
      while_addr_reg_0 x
    else do
      (sigs, _) <- lift get
      signal (outports sigs)

  where
    outports :: Sigs -> PortsOut
    outports s = PortsOut { ack_out = ack_reg s , data_out = data_out_reg s }

idle :: PortsIn -> ReT PortsIn PortsOut (StT (Sigs, Mem) I) ()
idle ip = let
             _go  = go ip
             _rnw = rnw ip
             p_i  = partition_in ip
             a_i  = addr_in ip
             d_i  = data_in ip
          in
            case (_go, _rnw) of
              (S, S) -> (lift (pre_read p_i a_i >> get) >>= connect) >>= perform_read
              (S, C) -> (lift (pre_write p_i a_i d_i >> get) >>= connect) >>= perform_write
              _      -> (lift (pre_idle >> get) >>= connect) >>= idle
          where

            pre_read :: W2 -> W10 -> StT (Sigs, Mem) I ()
            pre_read p_i a_i = do
              set_addr_reg (p_i <&> a_i)
              set_partition_reg p_i
              set_ack_reg C
              set_data_out_reg zeroW32

            pre_write :: W2 -> W10 -> W32 -> StT (Sigs, Mem) I ()
            pre_write p_i a_i d_i = do
              set_addr_reg (p_i <&> a_i)
              set_data_reg d_i
              set_partition_reg p_i
              set_ack_reg C
              set_data_out_reg zeroW32

            pre_idle :: StT (Sigs, Mem) I ()
            pre_idle = do
              set_ack_reg C
              set_data_out_reg zeroW32

perform_read :: PortsIn -> ReT PortsIn PortsOut (StT (Sigs, Mem) I) ()
perform_read _ = (lift (perform_read_act >> get) >>= connect) >>= idle
  where
    perform_read_act = do
      addr   <- get_addr_reg
      d_o_r' <- getloc addr
      set_data_out_reg d_o_r'
      set_ack_reg S

perform_write :: PortsIn -> ReT PortsIn PortsOut (StT (Sigs, Mem) I) ()
perform_write _ = (lift (perform_write_act >> get) >>= connect) >>= idle
  where
    perform_write_act = do
      set_data_out_reg zeroW32
      addr  <- get_addr_reg
      _data <- get_data_reg
      setloc addr _data
      set_ack_reg S

start :: ReT PortsIn PortsOut I ()
start = extrude reset (zeroSigs, zeroMem)

main = undefined
