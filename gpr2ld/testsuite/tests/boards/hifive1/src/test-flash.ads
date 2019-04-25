with System;                  use System;
with System.Storage_Elements; use System.Storage_Elements;

package Test is

   procedure Check_Memories;

   function In_RAM (Addr : System.Address) return Boolean
   is (To_Integer (Addr) in 16#80000000# .. 16#80000000# + 16 * 1024);

   function In_FLASH (Addr : System.Address) return Boolean
   is (To_Integer (Addr) in 16#20400000# .. 16#20400000# + 512 * 1024 * 1024);

end Test;
