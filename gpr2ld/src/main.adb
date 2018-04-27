with GNAT.Command_Line;
with GNAT.Strings;

with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.VFS;      use GNATCOLL.VFS;

with Setup;
with Utils;
with Device; use Device;

----------
-- Main --
----------

procedure Main is
   use GNAT.Strings;

   --  Spec representing the target device
   Spec : Device.Spec;

   use Setup;
   Input : aliased Command_Line_Values;

begin

   Input.Get_Arguments;

   Input.Display;

   Utils.Register_Memory_Map_Attributes;

   declare
      Tree : Project_Tree;

      Spec_File : constant Virtual_File :=
        Create_From_Base (Filesystem_String (Input.Project_File.all));

      Linker_Script : constant Virtual_File :=
        Create_From_Dir
         (Dir       => Create (Filesystem_String (Input.Output_Dir.all)),
          Base_Name => Filesystem_String (Input.Linker_File.all));

      Memory_Map : constant Virtual_File :=
        Create_From_Dir
         (Dir       => Create (Filesystem_String (Input.Output_Dir.all)),
          Base_Name => Filesystem_String (Input.Memory_Map_File.all));

      Startup_Code : constant Virtual_File :=
        Create_From_Dir
         (Dir       => Create (Filesystem_String (Input.Output_Dir.all)),
          Base_Name => Filesystem_String (Input.Startup_Code_File.all));

   begin
      Tree.Load
        (Root_Project_Path => Spec_File,
         Packages_To_Check => All_Packs);

      Spec.Get_Memory_List_From_Project (Tree.Root_Project);

      Spec.Get_Boot_Memory_From_Project (Tree.Root_Project);

      Spec.Get_CPU_From_Project (Tree.Root_Project);

      Spec.Generate_Sections;

      Spec.Dump_Linker_Script (Linker_Script);

      Spec.Dump_Memory_Map (Memory_Map);

      Spec.Dump_Startup_Code (Startup_Code);

      Spec.Display;
   end;
exception
   --  We catch the exception from the command line when
   --  the user called the executable with "-h" or "--help"
   when GNAT.Command_Line.Exit_From_Command_Line =>
      New_Line;

end Main;
