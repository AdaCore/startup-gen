with Ada.Text_IO; use Ada.Text_IO;

with Test;

procedure Main is
begin
   Put_Line ("=== Test for STM32F407 disco ===");
   Test.Check_Memories;
end Main;
