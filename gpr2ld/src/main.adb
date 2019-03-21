with GNAT.Command_Line;
with GNAT.Strings;

with Ada.Text_IO;           use Ada.Text_IO;

with GNATCOLL.Projects;     use GNATCOLL.Projects;
with GNATCOLL.VFS;          use GNATCOLL.VFS;

with Setup;
with Utils;
with Device;                use Device;

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
      Tree        : Project_Tree;

      Project_File : constant Virtual_File :=
        Create_From_Base (Filesystem_String (Input.Project_File.all));

   begin
      Tree.Load
        (Root_Project_Path => Project_File,
         Packages_To_Check => All_Packs);

      --  TODO: Put all that in a function that prepares the spec.
      Spec.Get_Memory_List_From_Project (Tree.Root_Project);

      Spec.Get_Boot_Memory_From_Project (Tree.Root_Project);

      Spec.Get_Interrupt_Vector_From_Project
         (Tree.Project_From_Name ("interruptions"));

      Spec.Get_CPU_From_Project (Tree.Root_Project);

      Spec.Validate;
      --  TODO End of setup.

      if Input.Linker_File /= null then
         Spec.Dump_Linker_Script (Input.Linker_File.all);
      end if;

      if Input.Startup_Code_File /= null then
         Spec.Dump_Startup_Code (Input.Startup_Code_File.all);
      end if;

      Spec.Display;
   end;
exception
   --  We catch the exception from the command line when
   --  the user called the executable with "-h" or "--help"
   when GNAT.Command_Line.Exit_From_Command_Line =>
      New_Line;

end Main;
