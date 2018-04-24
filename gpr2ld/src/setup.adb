with Ada.Text_IO;       use Ada.Text_IO;
with GNAT.Command_Line; use GNAT.Command_Line;

-----------
-- Setup --
-----------

package body Setup is

   -------------------
   -- Get_Arguments --
   -------------------

   procedure Get_Arguments (Values : aliased out Command_Line_Values)
   is
      Config : Command_Line_Configuration;
   begin

      Define_Switch
        (Config,
         Values.Output_Dir'Access,
         "-o:",
         Help => "Directory in which generated files will be put.");

      Define_Switch
        (Config,
         Values.Linker_File'Access,
         "-l:",
         Help => "Name of the generated linker script.");

      Getopt (Config);

      Values.Config_File := new String'(Get_Argument);

      Put_Line ("Config " & Values.Config_File.all);
      Put_Line ("Out Dir " & Values.Output_Dir.all);
      Put_Line ("Linker Script " & Values.Linker_File.all);

   end Get_Arguments;

end Setup;
