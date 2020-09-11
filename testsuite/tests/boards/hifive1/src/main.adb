with Text_IO; use Text_IO;

with Test;

procedure Main is
begin
   Put_Line ("=== Test for RISC-V32 HiFive1 ===");
   Test.Check_Memories;
end Main;
