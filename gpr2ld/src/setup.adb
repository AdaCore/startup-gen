with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Command_Line; use GNAT.Command_Line;

package body Setup is

   procedure Get_Arguments (Config_File,
      Linker_File, Output_Dir : aliased out String_Access) is

      Config : Command_Line_Configuration;
   begin

      Define_Switch (Config, Output_Dir'Access, "-o:",
         Help => "Directory in which generated files will be put.");

      Define_Switch (Config, Linker_File'Access, "-l:",
         Help => "Name of the generated linker script.");

      Getopt (Config);

      Config_File := new String'(Get_Argument);

      Put_Line ("Config " & Config_File.all);
      Put_Line ("Out Dir " & Output_Dir.all);
      Put_Line ("Linker Script " & Linker_File.all);

   end Get_Arguments;

end Setup;
