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
          Values.Architecture_File'Access,
          "-c:",
          Help => "Path to the project file containing " &
                  "the mapping of processors to their architecture.");

      Define_Switch
         (Config,
          Values.Linker_File'Access,
          "-l:",
          Help => "Name of the generated linker script.");

      Define_Switch
         (Config,
          Values.Startup_Code_File'Access,
          "-s:",
          Help => "Name of the generated startup code.");

      Getopt (Config);

      Values.Project_File := new String'(Get_Argument);
   end Get_Arguments;

   -------------
   -- Display --
   -------------

   procedure Display (Values : in out Command_Line_Values)
   is
   begin
      Put_Line ("Spec " & Values.Project_File.all);
      Put_Line ("Out Dir " & Values.Output_Dir.all);
      Put_Line ("Linker Script " & Values.Linker_File.all);
      Put_Line ("Configuration file " &  Values.Architecture_File.all);
   end Display;

end Setup;
