with Interfaces;
with System;

package body Text_IO is

   use type Interfaces.Unsigned_8;

   Base_Address    : constant := 16#2000_0000#;

   Data : Interfaces.Unsigned_8
     with Address => System'To_Address (base_address + 16#00#);

   LSR  : Interfaces.Unsigned_8
     with Address => System'To_Address (base_address + 16#14#);

   LSR_THRE : constant Interfaces.Unsigned_8 := 2#0010_0000#;
   LSR_DR   : constant Interfaces.Unsigned_8 := 2#0000_0001#;

   procedure Put (C : Character);

   -----------------
   -- Is_Tx_Ready --
   -----------------

   function Is_Tx_Ready return Boolean is
   begin
      return (LSR and LSR_THRE) /= 0;
   end Is_Tx_Ready;


   ---------
   -- Put --
   ---------

   procedure Put (C : Character) is
   begin
      while not Is_Tx_Ready loop
         null;
      end loop;
      Data := Character'Pos (C);
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (Str : String) is
   begin
      for C of Str loop
         Put (C);
      end loop;
      Put (ASCII.CR);
      Put (ASCII.LF);
   end Put_Line;
end Text_IO;
