with Interfaces; use Interfaces;
with System;
with System.Machine_Code; use System.Machine_Code;

with Ada.Text_IO; use Ada.Text_IO;

with Test;

procedure Main is

   Mss_Reset_Cr : Unsigned_32
     with Address => System'To_Address (16#20002018#);

begin
   Put_Line ("=== Test for RISC-V64 PolarFire SOC ===");

   --  The FPU should be enabled here, do a simple float operation here to
   --  check.
   Asm ("fadd.s f0, f1, f0",
        Volatile => True);

   Test.Check_Memories;

   Mss_Reset_Cr := 16#DEAD#;

end Main;
