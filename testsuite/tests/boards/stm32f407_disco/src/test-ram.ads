with Interfaces;              use Interfaces;
with System;                  use System;
with System.Storage_Elements; use System.Storage_Elements;

package Test is

   Start : constant Unsigned_32;
   pragma Import (C, Start, "Reset_Handler");

   --  This vector inserted in the flash_rodata section is supposed to be
   --  allocated at the address zero, and will be used by QEMU to find the
   --  reset PC address and stack pointer.
   Boot_Vect : constant array (1 .. 2) of Address :=
     (Null_Address,  -- Stack pointer
      Start'Address) -- Reset address
     with Linker_Section => ".flash_rodata";

   procedure Check_Memories;

   function In_CCM (Addr : System.Address) return Boolean
   is (To_Integer (Addr) in 16#10000000# .. 16#10000000# + 64 * 1024);

   function In_SRAM (Addr : System.Address) return Boolean
   is (To_Integer (Addr) in 16#20000000# .. 16#20000000# + 128 * 1024);

   function In_FLASH (Addr : System.Address) return Boolean
   is (To_Integer (Addr) in 16#08000000# .. 16#08000000# + 1024 * 1024);

end Test;
