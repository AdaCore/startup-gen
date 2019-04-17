with System;                  use System;
with System.Storage_Elements; use System.Storage_Elements;

package Test is

   procedure Check_Memories;

   function In_CCM (Addr : System.Address) return Boolean
   is (To_Integer (Addr) in 16#10000000# .. 16#10000000# + 64 * 1024);

   function In_SRAM (Addr : System.Address) return Boolean
   is (To_Integer (Addr) in 16#20000000# .. 16#20000000# + 128 * 1024);

   function In_FLASH (Addr : System.Address) return Boolean
   is (To_Integer (Addr) in 16#08000000# .. 16#08000000# + 1024 * 1024);

end Test;
