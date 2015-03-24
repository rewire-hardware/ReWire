--
--	Package File Template
--
--	Purpose: This package defines supplemental types, subtypes, 
--		 constants, and functions 
--
--   To use any of the example code shown below, uncomment the lines and modify as necessary
--

library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;

package prims is

  pure function zeroW8 return std_logic_vector;
  pure function oneW8 return std_logic_vector;
  pure function eqW8 (arg_0 : std_logic_vector ; arg_1 : std_logic_vector) return std_logic_vector;
  pure function rolW8 (arg_0 : std_logic_vector) return std_logic_vector;
  pure function rorW8 (arg_0 : std_logic_vector) return std_logic_vector;
  pure function msbW8 (arg_0 : std_logic_vector) return std_logic_vector;
  pure function lsbW8 (arg_0 : std_logic_vector) return std_logic_vector;
  pure function notW8 (arg_0 : std_logic_vector) return std_logic_vector;
  pure function andW8 (arg_0 : std_logic_vector ; arg_1 : std_logic_vector) return std_logic_vector;
  pure function orW8 (arg_0 : std_logic_vector ; arg_1 : std_logic_vector) return std_logic_vector;
  pure function xorW8 (arg_0 : std_logic_vector ; arg_1 : std_logic_vector) return std_logic_vector;
  pure function notBit (arg_0 : std_logic_vector) return std_logic_vector;
  pure function plusW8 (arg_0 : std_logic_vector ; arg_1 : std_logic_vector) return std_logic_vector;
  pure function shrCW8(arg_0 : std_logic_vector ; arg_1 : std_logic_vector) return std_logic_vector;
  pure function shlCW8(arg_0 : std_logic_vector ; arg_1 : std_logic_vector) return std_logic_vector;
  pure function plusCW8 (arg_0 : std_logic_vector ; -- W8
                         arg_1 : std_logic_vector ; -- W8
								 arg_2 : std_logic_vector)  -- Bit
								 return std_logic_vector;
  pure function minusCW8 (arg_0 : std_logic_vector ; -- W8
                          arg_1 : std_logic_vector ; -- W8
	 	  						  arg_2 : std_logic_vector)  -- Bit
	 							  return std_logic_vector;						 
--  pure function 
  pure function rotl (arg_0 : std_logic_vector) return std_logic_vector;
  pure function plusOne (arg_0 : std_logic_vector) return std_logic_vector;
  pure function prim_and(arg_0 : std_logic_vector ; arg_1 : std_logic_vector)
    return std_logic_vector;
  pure function prim_ceq(arg_0 : std_logic_vector ; arg_1 : std_logic_vector)
    return std_logic_vector;
  pure function prim_not(arg_0 : std_logic_vector)
    return std_logic_vector;
  pure function prim_or(arg_0 : std_logic_vector ; arg_1 : std_logic_vector)
    return std_logic_vector;
end prims;

package body prims is

  pure function zeroW8 return std_logic_vector is begin return "00000000"; end zeroW8;
  pure function oneW8 return std_logic_vector is begin return "00000001"; end oneW8;

  pure function eqW8 (arg_0 : std_logic_vector ; arg_1 : std_logic_vector) return std_logic_vector
  is
  begin
    if arg_0 = arg_1 then return "1"; else return "0"; end if;
  end eqW8;
  
  pure function rolW8 (arg_0 : std_logic_vector) return std_logic_vector
  is
  begin
    return (arg_0(1 to 7) & arg_0(0 to 0));
  end rolW8;

  pure function rorW8 (arg_0 : std_logic_vector) return std_logic_vector
  is
  begin
    return (arg_0(7 to 7) & arg_0 (0 to 6));
  end rorW8;
  
  pure function msbW8 (arg_0 : std_logic_vector) return std_logic_vector
  is
  begin
    return (arg_0(arg_0'left to arg_0'left));
  end msbW8;
  
  pure function lsbW8 (arg_0 : std_logic_vector) return std_logic_vector
  is
  begin
    return (arg_0(arg_0'right to arg_0'right));
  end lsbW8;
  
  pure function notW8 (arg_0 : std_logic_vector) return std_logic_vector
  is
  begin
    return (not arg_0);
  end notW8;

  pure function andW8 (arg_0 : std_logic_vector ; arg_1 : std_logic_vector) return std_logic_vector
  is
  begin
    return (arg_0 AND arg_1);
  end andW8;

  pure function orW8 (arg_0 : std_logic_vector ; arg_1 : std_logic_vector) return std_logic_vector
  is
  begin
    return (arg_0 OR arg_1);
  end orW8;

  pure function xorW8 (arg_0 : std_logic_vector ; arg_1 : std_logic_vector) return std_logic_vector
  is
  begin
    return (arg_0 XOR arg_1);
  end xorW8;

  pure function notBit (arg_0 : std_logic_vector) return std_logic_vector
  is
  begin
    return (NOT arg_0);
  end notBit;

  pure function plusW8 (arg_0 : std_logic_vector ; arg_1 : std_logic_vector) return std_logic_vector
  is
  begin
    return (std_logic_vector(unsigned("0"&arg_0)+unsigned("0"&arg_1)));
  end plusW8;

  pure function shrCW8(arg_0 : std_logic_vector ; arg_1 : std_logic_vector) return std_logic_vector
  is
    variable regOut : std_logic_vector(0 to 7);
	 variable carryOut : std_logic_vector(0 to 0);
  begin
    carryOut := arg_0(7 to 7);
	 regOut   := arg_1 & (arg_0(0 to 6));
	 return (carryOut & regOut);
  end shrCW8;

  pure function shlCW8(arg_0 : std_logic_vector ; arg_1 : std_logic_vector) return std_logic_vector
  is
    variable regOut : std_logic_vector(0 to 7);
	 variable carryOut : std_logic_vector(0 to 0);
  begin
    carryOut := arg_0(0 to 0);
	 regOut   := (arg_0(1 to 7) & arg_1);
	 return (carryOut & regOut);
  end shlCW8;
  
  pure function plusCW8 (arg_0 : std_logic_vector ; -- W8
                         arg_1 : std_logic_vector ; -- W8
								 arg_2 : std_logic_vector)  -- Bit
								 return std_logic_vector
  is
    variable result : std_logic_vector(0 to 8);
  begin
    result := std_logic_vector(unsigned("0"&arg_0)+unsigned("0"&arg_1)+unsigned("00000000"&arg_2));
	 return result;
  end plusCW8;

  pure function minusCW8 (arg_0 : std_logic_vector ; -- W8
                          arg_1 : std_logic_vector ; -- W8
								  arg_2 : std_logic_vector)  -- Bit
								  return std_logic_vector
  is
    variable result : std_logic_vector(0 to 8);
  begin
    result := std_logic_vector(unsigned("0"&arg_0)-unsigned("0"&arg_1)-unsigned("00000000"&arg_2));
	 return result;
  end minusCW8;
  
  pure function rotl (arg_0 : std_logic_vector) return std_logic_vector
  is
    variable l : std_logic_vector(0 to 7) := arg_0;
  begin
    return (l(1 to 7) & l(0 to 0));
  end rotl;
  
  pure function plusOne (arg_0 : std_logic_vector) return std_logic_vector
  is
    variable n : std_logic_vector(0 to 7) := arg_0;
  begin
    return (std_logic_vector(unsigned(n)+1));
  end plusOne;
  pure function prim_and(arg_0 : std_logic_vector ; arg_1 : std_logic_vector)
    return std_logic_vector
  is begin return arg_0 AND arg_1; end prim_and;
  pure function prim_ceq(arg_0 : std_logic_vector ; arg_1 : std_logic_vector)
    return std_logic_vector
  is begin if arg_0 = arg_1 then return "1"; else return "0"; end if; end prim_ceq;
  pure function prim_not(arg_0 : std_logic_vector)
    return std_logic_vector
  is begin return (NOT arg_0); end prim_not;
  pure function prim_or(arg_0 : std_logic_vector ; arg_1 : std_logic_vector)
    return std_logic_vector
  is begin return arg_0 OR arg_1; end prim_or;
  
---- Example 1
--  function <function_name>  (signal <signal_name> : in <type_declaration>  ) return <type_declaration> is
--    variable <variable_name>     : <type_declaration>;
--  begin
--    <variable_name> := <signal_name> xor <signal_name>;
--    return <variable_name>; 
--  end <function_name>;

---- Example 2
--  function <function_name>  (signal <signal_name> : in <type_declaration>;
--                         signal <signal_name>   : in <type_declaration>  ) return <type_declaration> is
--  begin
--    if (<signal_name> = '1') then
--      return <signal_name>;
--    else
--      return 'Z';
--    end if;
--  end <function_name>;

---- Procedure Example
--  procedure <procedure_name>  (<type_declaration> <constant_name>  : in <type_declaration>) is
--    
--  begin
--    
--  end <procedure_name>;
 
end prims;
