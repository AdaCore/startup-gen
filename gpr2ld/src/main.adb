with Device; use Device;

with GNAT.Command_Line;
with GNAT.Strings;

with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
   use GNAT.Command_Line;
   use GNAT.Strings;

   --  Spec representing the target device
   Spec : Device.Spec;

   --  Path to the GPR configuration file
   Config_File : String_Access;

   --  Directory in which will be put all the generated files.
   --  Output_Dir : aliased String_Access;

   Config : Command_Line_Configuration;

   procedure Setup (Switch, Param, Section : String);

   procedure Setup (Switch, Param, Section : String) is
   begin
      if Switch = "-c" then
         Config_File := new String'(Get_Argument);
      else
         Put_Line ("Option not recognized.");
      end if;
      Put_Line ("Param " & Param);
      Put_Line ("Section " & Section);
   end Setup;

begin

   Define_Switch (Config, "-c",
            Help => "<file> GPR file that contains the device configuration");

   Getopt (Config, Setup'Unrestricted_Access);

   Register_Memory_Map_Attributes;

   Spec.Set_Memory_List (Config_File.all);

   Put_Line ("Hello GPR World");
  -- exception
  --    when others =>
  --          New_Line;
end Main;
