with Interfaces; use Interfaces;
with System;
with System.Machine_Code; use System.Machine_Code;

with Ada.Text_IO; use Ada.Text_IO;

with Test;

procedure Main is
begin
   Put_Line ("=== Test for RISC-V64 PolarFire SOC ===");

   --  The FPU should be enabled here, do a simple float operation here to
   --  check.
   Asm ("fadd.s f0, f1, f0",
        Volatile => True);

   Test.Check_Memories;
end Main;
