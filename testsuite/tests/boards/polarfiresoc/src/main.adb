with Interfaces; use Interfaces;
with System;

with Ada.Text_IO; use Ada.Text_IO;

with Test;

procedure Main is

   Test_Device : Unsigned_32
     with Address => System'To_address (16#100000#);

begin
   Put_Line ("=== Test for RISC-V64 PolarFire SOC ===");
   Test.Check_Memories;

   Test_Device := 16#5555#;

end Main;
