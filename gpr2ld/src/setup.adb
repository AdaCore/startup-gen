with Ada.Text_IO;       use Ada.Text_IO;
with GNAT.Command_Line; use GNAT.Command_Line;
with Utils;

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
          Values.Linker_File'Access,
          "-l:",
          Help => "Name of the generated linker script.");

      Define_Switch
         (Config,
          Values.Startup_Code_File'Access,
          "-s:",
          Help => "Name of the generated startup code.");

      Define_Switch
         (Config,
          Values.Project_File'Access,
          "-P:",
          Help => "Name of the project file with the device configuation.");

      Getopt (Config);

      if Values.Project_File = null or else Values.Project_File.all = "" then
         Utils.Fatal_Error ("Project file required (-P)");
      end if;
   end Get_Arguments;

   -------------
   -- Display --
   -------------

   procedure Display (Values : in out Command_Line_Values)
   is
   begin
      Put_Line ("Spec " & Values.Project_File.all);
      Put_Line ("Linker Script " & Values.Linker_File.all);
      Put_Line ("Startup Code " & Values.Startup_Code_File.all);
   end Display;

end Setup;
