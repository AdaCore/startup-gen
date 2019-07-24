with FE310_SVD.UART; use FE310_SVD.UART;
with FE310_SVD.GPIO; use FE310_SVD.GPIO;
with HAL;            use HAL;

package body Text_IO is

   procedure Put (C : Character);

   ---------
   -- Put --
   ---------

   procedure Put (C : Character) is
   begin
      while UART0_Periph.TXDATA.FULL loop
         null;
      end loop;
      UART0_Periph.TXDATA.DATA := Character'Pos (C);
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

begin

   GPIO0_Periph.IO_FUNC_SEL.Arr (16) := False;
   GPIO0_Periph.IO_FUNC_SEL.Arr (17) := False;

   GPIO0_Periph.IO_FUNC_EN.Arr (16) := True;
   GPIO0_Periph.IO_FUNC_EN.Arr (17) := True;

   UART0_Periph.DIV.DIV := UInt16 ((278895001 / 115200)) - 1;
   UART0_Periph.TXCTRL.ENABLE := True;
   UART0_Periph.RXCTRL.ENABLE := True;

   for I in 1 .. 1_000 loop
      null;
   end loop;

end Text_IO;
