with Interfaces; use Interfaces;
with System;
with System.Machine_Code; use System.Machine_Code;

with Ada.Text_IO; use Ada.Text_IO;

with Test;

procedure Main is

   Test_Device : Unsigned_32
     with Address => System'To_Address (16#100000#);

begin
   Put_Line ("=== Test for RISC-V64 PolarFire SOC ===");

   --  The FPU should be enabled here, do a simple float operation here to
   --  check.
   Asm ("fadd.s f0, f1, f0",
        Volatile => True);

   Test.Check_Memories;

   Test_Device := 16#5555#;

end Main;
