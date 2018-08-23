with GNAT.Strings;      use GNAT.Strings;
with Ada.IO_Exceptions; use Ada.IO_Exceptions;
with GNATCOLL.VFS;      use GNATCOLL.VFS;

-----------
-- Setup --
-----------

package Setup is

   type Command_Line_Values is tagged record
      Project_File      : aliased String_Access := null;
      Architecture_File : aliased String_Access :=
        new String'(Display_Full_Name
         (Get_Parent (Locate_On_Path
            (Base_Name => "gpr2ld"))) & "configuration.gpr");
      Linker_File       : aliased String_Access :=
         new String'(Display_Full_Name (Get_Current_Dir / "linker.ld"));
      Startup_Code_File : aliased String_Access :=
         new String'(Display_Full_Name (Get_Current_Dir / "startup.s"));
   end record;

   --  We verify that the mandatory arguments are correct.
   procedure Get_Arguments (Values : aliased out Command_Line_Values)
      with Post => (Values.Project_File.all /= "")
     or else raise Name_Error
        with "You must pass a file on the command line.";

   procedure Display (Values : in out Command_Line_Values);

end Setup;
