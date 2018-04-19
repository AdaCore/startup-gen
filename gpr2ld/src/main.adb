with GNAT.Command_Line;
with GNAT.Strings;

with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.VFS; use GNATCOLL.VFS;

with Setup;
with Utils;
with Device; use Device;

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

   Setup.Get_Arguments (Config, Config_File, Output_Dir);

   Utils.Register_Memory_Map_Attributes;

   declare
      VFS : constant Virtual_File := Create_From_Base
                        (Filesystem_String (Config_File.all));
      Tree : Project_Tree;

      --  TODO: read linker script name from cmdline
      Linker_Script : constant Virtual_File := Create_From_Base
                        (Filesystem_String (String'("linker.ld")));

   begin
      Tree.Load (Root_Project_Path => VFS,
                 Packages_To_Check => All_Packs);

      Spec.Get_Memory_List_From_Project (Tree.Root_Project);

      Spec.Get_CPU_From_Project (Tree.Root_Project);

      Spec.Dump_Linker_Script (Linker_Script);

      Spec.Display;
   end;
   exception
      --  We catch the exception from the command line the user called the
      --  executable with "-h" or "--help"
      when GNAT.Command_Line.Exit_From_Command_Line => New_Line;

end Main;
