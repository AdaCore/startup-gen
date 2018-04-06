with Ada.Text_IO; use Ada.Text_IO;
with Device; use Device;

procedure Main is
   Spec : Device.Spec;
begin
   Register_Memory_Map_Attributes;
   Set_Memory_List (Spec, "memory.gpr");
   Put_Line ("Hello GPR World");
end Main;
