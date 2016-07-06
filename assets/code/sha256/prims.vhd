library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package prims is
  pure function w32Plus (x : std_logic_vector; y : std_logic_vector) return std_logic_vector;
  pure function w32Xor (x : std_logic_vector; y : std_logic_vector) return std_logic_vector;
  pure function w32And (x : std_logic_vector; y : std_logic_vector) return std_logic_vector;
  pure function w32Not (x : std_logic_vector) return std_logic_vector;
  pure function incCtr (x : std_logic_vector) return std_logic_vector;
end prims;

package body prims is
  pure function w32Plus (x : std_logic_vector; y : std_logic_vector) return std_logic_vector is
  begin
    return (std_logic_vector(unsigned(x)+unsigned(y)));
  end w32Plus;

  pure function w32Xor (x : std_logic_vector; y : std_logic_vector) return std_logic_vector is
  begin
    return (x xor y);
  end w32Xor;

  pure function w32And (x : std_logic_vector; y : std_logic_vector) return std_logic_vector is
  begin
    return (x and y);
  end w32And;
  
  pure function w32Not (x : std_logic_vector) return std_logic_vector is
  begin
    return (not x);
  end w32Not;

  pure function incCtr (x : std_logic_vector) return std_logic_vector is
  begin
    return (std_logic_vector(unsigned(x)+1));
  end incCtr;
end prims;
