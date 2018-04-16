with Device; use Device;

with GNAT.Command_Line;
with GNAT.Strings;
with Ada.Text_IO; use Ada.Text_IO;

with Setup;
with Utils;

--  with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
   use GNAT.Command_Line;
   use GNAT.Strings;

   --  Spec representing the target device
   Spec : Device.Spec;

   --  Path to the GPR configuration file
   Config_File : aliased String_Access;

   --  Directory in which will be put all the generated files.
   --  TODO: default value must be os independant.
   Output_Dir : aliased String_Access := new String'("./");

   Config : Command_Line_Configuration;

begin

   --  Is not the same as Get_arguments provided by GNAT.Command_Line.
   Setup.Get_Arguments (Config, Config_File, Output_Dir);

   Utils.Register_Memory_Map_Attributes;

   Spec.Set_Memory_List (Config_File.all);

   Spec.Dump;

   exception
      --  We catch the exception from the command line the user called the
      --  executable with "-h" or "--help"
      when GNAT.Command_Line.Exit_From_Command_Line => New_Line;

end Main;
