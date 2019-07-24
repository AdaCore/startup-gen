with Interfaces;              use Interfaces;
with System;                  use System;
with System.Storage_Elements; use System.Storage_Elements;

package Test is

   Start : constant Unsigned_32;
   pragma Import (C, Start, "_start");

   --  These few instructions inserted in the flash_rodata sectioareis supposed
   --  to be allocated at the start of flash memory, and will jump to the _start
   --  symbol.
   Boot_Vect : constant array (1 .. 4) of Address :=
     (System'To_Address (2#00000000000000000000_00001_0010111#),   -- auipc ra,0x0
      System'To_Address (2#000000001100_00001_010_00001_0000011#), -- lw    ra,12(ra)
      System'To_Address (2#000000000000_00001_000_00001_1100111#), -- jalr  ra
      Start'Address)
     with Linker_Section => ".flash_rodata";

   procedure Check_Memories;

   function In_RAM (Addr : System.Address) return Boolean
   is (To_Integer (Addr) in 16#80000000# .. 16#80000000# + 16 * 1024);

   function In_FLASH (Addr : System.Address) return Boolean
   is (To_Integer (Addr) in 16#20400000# .. 16#20400000# + 512 * 1024 * 1024);

end Test;
