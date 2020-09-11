with Interfaces;              use Interfaces;
with System;                  use System;
with System.Storage_Elements; use System.Storage_Elements;

package Test is

   procedure Check_Memories;

   function In_RAM (Addr : System.Address) return Boolean
   is (To_Integer (Addr) in 16#80000000# .. 16#80000000# + 128 * 1024 * 1024);

end Test;
